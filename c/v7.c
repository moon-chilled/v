#include "v.h"

static s7_pointer s7_make_v_function(VV *vv, Function f) {
	switch (f.type.type) {
		case TypeStr: return s7_make_string_with_length(vv->s, (const char*)f.str.s, f.str.l);
		case TypeMotion:   return s7_make_c_object(vv->s, vv->tag_function_motion,   cnew(f));
		case TypeMutation: return s7_make_c_object(vv->s, vv->tag_function_mutation, cnew(f));
		case TypeFunction: return s7_make_c_object(vv->s, vv->tag_function_function, cnew(f));
		default: assert(0);
	}
}

static Function *s7_to_v_function(VV *vv, s7_pointer p) {
	s7_int t = s7_c_object_type(p); //returns -1 for non-c-objects, so no problem if it's something else entirely
	if (t != vv->tag_function_function && t != vv->tag_function_mutation && t != vv->tag_function_motion) {
		if (s7_is_string(p)) {
			//ugh, cnew
			return cnew((Function){.type.type=TypeStr, .str={.s=(const u1*)s7_string(p), .l=s7_string_length(p)}});
		}
		return NULL;
	}
	return s7_c_object_value(p);
}

struct s7_mutation { s7_pointer prepare, perform, undo; };

static Loc motion_perform(const V *v, const void *state) {
	s7_pointer f = (s7_pointer)state; //const correctness, thou art a foul temptress!

	s7_pointer loc = s7_call(v->vv->s, f, s7_nil(v->vv->s));
	Loc *ret = s7_c_object_value_checked(loc, v->vv->tag_loc);

	if (!ret) {
		msg((V*)v, "scheme error: object '%s' was unexpectedly not a location", s7_object_to_c_string(v->vv->s, loc));
		return v->b.loc;
	}

	return *ret;
}

static void s7_mutation_prepare(const V *v, void **state) {
	struct s7_mutation *m = *state;
	s7_call(v->vv->s, m->prepare, s7_nil(v->vv->s));
}
static void s7_mutation_perform(V *v, const void *state) {
	const struct s7_mutation *m = state;
	s7_call(v->vv->s, m->perform, s7_nil(v->vv->s));
}
static void s7_mutation_undo(V *v, const void *state) {
	const struct s7_mutation *m = state;
	s7_call(v->vv->s, m->undo, s7_nil(v->vv->s));
}

static Function s7_function_transform(const V *v, void *state, const Function *other) {
	Function *ret = s7_to_v_function(v->vv, s7_call(v->vv->s, state, s7_list(v->vv->s, 1, s7_make_v_function(v->vv, *other))));
	if (!ret) {
		msg((V*)v, "scheme error: function did not return a function");
		assert(0);
		return new_str(NULL, 0);
	}
	return *ret;
}

static bool s7_signature_to_type(VV *vv, s7_pointer signature, Type *out) {
	if (s7_is_pair(signature)) {
		if (s7_is_null(vv->s, s7_cdr(signature))) {
			signature = s7_car(signature);
			goto solo;
		}
		out->type = TypeFunction;
		out->fn = new(Type, 2);
		if (!s7_signature_to_type(vv, s7_car(signature), out->fn + 0)) return false;
		if (!s7_signature_to_type(vv, s7_cdr(signature), out->fn + 1)) return false;
		return true;
	} else if (s7_is_symbol(signature)) {
solo:
		if (s7_is_eq(signature, vv->sym_bottom)) out->type = TypeBottom;
		else if (s7_is_eq(signature, vv->sym_str)) out->type = TypeStr;
		else if (s7_is_eq(signature, vv->sym_motion)) out->type = TypeMotion;
		else if (s7_is_eq(signature, vv->sym_mutation)) out->type = TypeMutation;
		else if (s7_is_eq(signature, vv->sym_function)) out->type = TypeFunction;
		else if (s7_is_eq(signature, vv->sym_top)) out->type = TypeTop;
		else return false;
		return true;
	}
	return false;
}

#define PRELUDE s7_pointer __attribute__((unused)) _original_args = args; int __attribute__((unused)) _argument_number = 1;
#define PPRELUDE(fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t)
#define PSPRELUDE(fn, t_, t) if (!s7_is_pair(args) || !t_(s, s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t)
#define GPOP(x, fn) if (!s7_is_pair(args)) return s7_wrong_number_of_args_error(s, fn, _original_args); s7_pointer x = s7_car(args); args = s7_cdr(args); _argument_number++
#define POP(x, fn, t_, t) PPRELUDE(fn, t_, t); s7_pointer x = s7_car(args); args = s7_cdr(args); _argument_number++
#define TPOP(T, x, get, fn, t_, t) PPRELUDE(fn, t_, t); T x = get(s7_car(args)); args = s7_cdr(args); _argument_number++
#define CPPOP(T, x, fn, tag) PPRELUDE(fn, s7_is_c_object, "c object"); T *x = s7_c_object_value_checked(s7_car(args), tag); if (!(x)) { char buf[128]; sprintf(buf, "c object of type '%lli'", (long long)(tag)); } args = s7_cdr(args); _argument_number++
#define STPOP(T, x, get, fn, t_, t) PPRELUDE(fn, t_, t); T x = get(s, s7_car(args)); args = s7_cdr(args); _argument_number++
#define IPOP(x, fn) TPOP(s7_int, x, s7_integer, fn, s7_is_integer, "integer")
#define RPOP(x, min, max, fn) PPRELUDE(fn, s7_is_integer, "integer"); s7_int x = s7_integer(s7_car(args)); if (x < (s7_int)(min)) { char buf[128]; sprintf(buf, ">=%lli", (long long)(min)); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); }  if (x >= (s7_int)(max)) { char buf[128]; sprintf(buf, "<%lli", (long long)(max)); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); } args = s7_cdr(args); _argument_number++
#define URPOP(x, max, fn) RPOP(x ## _tmp, 0, max, fn); usz x = x ## _tmp
#define UPOP(x, fn) URPOP(x, 1ll << 62, fn)
#define CPOP(x, fn) TPOP(u1, x, s7_character, fn, s7_is_character, "character")
#define PPOP(x, fn) POP(x, fn, s7_is_procedure, "procedure")
#define BPOP(x, fn) STPOP(bool, x, s7_boolean, fn, s7_is_boolean, "boolean")

static bool sym_to_mode(VV *vv, s7_pointer sym, Mode *mode) {
	if (s7_is_eq(sym, vv->sym_insert)) *mode = ModeInsert;
	else if (s7_is_eq(sym, vv->sym_motion)) *mode = ModeMotion;
	else if (s7_is_eq(sym, vv->sym_mutation)) *mode = ModeMutate;
	else if (s7_is_eq(sym, vv->sym_function)) *mode = ModeFunction;
	else if (s7_is_eq(sym, vv->sym_normal)) *mode = ModeNormal;
	else return false;
	return true;
}
static s7_pointer mode_to_sym(Mode m, VV *vv) {
	switch (m) {
		case ModeInsert: return vv->sym_insert;
		case ModeMotion: return vv->sym_motion;
		case ModeMutate: return vv->sym_mutation;
		case ModeFunction: return vv->sym_function;
		case ModeNormal: return vv->sym_normal;
		default: assert(0);
	}
}


struct s7_hi_env { s7_pointer hi, conv; };
static struct hi_ret s7_highlight(VV *vv, void *env, s7_pointer old_state, TextBufferIter *tbi) {
	struct s7_hi_env *e = env;
	s7_pointer new_state = s7_call(vv->s, e->hi, s7_cons(vv->s, s7_make_c_pointer_with_type(vv->s, tbi, vv->sym_text_buffer_iter, s7_nil(vv->s)), s7_cons(vv->s, old_state, s7_nil(vv->s))));
	s7_pointer clr = s7_call(vv->s, e->conv, s7_cons(vv->s, new_state, s7_nil(vv->s)));
	assert(s7_is_integer(clr));
	return (struct hi_ret){s7_integer(clr), new_state};
}



static s7_pointer set_highlighter(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_set_highlighter "(set-highlighter init-state state-step conv) sets the syntax highlighter for the current v"
#define Q_set_highlighter s7_make_signature(vv->s, 3, vv->sym_not, vv->sym_procedure_p, vv->sym_procedure_p)
	GPOP(init, "set-highlighter");
	PPOP(highlight, "set-highlighter");
	PPOP(conv, "set-highlighter");

	Highlighter nh = {.env=onew(struct s7_hi_env, .hi=highlight, .conv=conv), .init_state=init, .highlight=s7_highlight};
	vv->v->highlighter = nh;

	return s7_f(s);
}

//todo check aritability of functions

// type -> function -> cobject
static s7_pointer make_higher_order_function(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_make_higher_order_function "(LOW-make-higher-order-function mode signature fn) makes a higher-order function out of fn with signature signature"
#define Q_make_higher_order_function s7_make_signature(vv->s, 3, vv->sym_c_object_p, vv->sym_list_p, vv->sym_procedure_p)
	POP(smode, "LOW-make-higher-order-function", s7_is_symbol, "symbol");
	Mode mode;
	if (!sym_to_mode(vv, smode, &mode)) {
		return s7_wrong_type_arg_error(s, "LOW-make-higher-order-function", 1, smode, "a symbol: one of insert, motion, mutation, or function)");
	}

	Type type;
	POP(sig, "LOW-make-higher-order-function", s7_is_pair, "pair");
	if (!s7_signature_to_type(vv, s7_reverse(s, sig), &type)) {
		return s7_wrong_type_arg_error(s, "LOW-make-higher-order-function", 2, sig, "a signature");
	}
	PPOP(f, "LOW-make-higher-order-function");
	Function r = (Function){
		.type = type,
		.function = {
			.state = f,
			.mode = mode,
			.transform = s7_function_transform,
		},
	};
	return s7_make_v_function(vv, r);
}

// function -> cobject
static s7_pointer make_motion(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_make_motion "(LOW-make-motion fn) makes a motion out of fn"
#define Q_make_motion s7_make_signature(vv->s, 2, vv->sym_c_object_p, vv->sym_procedure_p)
	PPOP(f, "LOW-make-motion");

	Function r = new_motion(f, motion_perform);
	return s7_make_v_function(vv, r);
}

// function -> function -> function -> cobject
static s7_pointer make_mutation(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_make_mutation "(LOW-make-mutation prepare perform undo) makes a mutation out of its parameters"
#define Q_make_mutation s7_make_signature(vv->s, 2, vv->sym_c_object_p, vv->sym_procedure_p, vv->sym_procedure_p, vv->sym_procedure_p)
	PPOP(prepare, "LOW-make-motion");
	PPOP(perform, "LOW-make-motion");
	PPOP(undo, "LOW-make-motion");

	Function r = new_mutation(onew(struct s7_mutation, .prepare=prepare, .perform=perform, .undo=undo), s7_mutation_prepare, s7_mutation_perform, s7_mutation_undo);
	return s7_make_v_function(vv, r);
}

// symbol (mode) -> character -> cobject -> nil
static s7_pointer create_binding(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_create_binding "(LOW-create-binding mode character-or-special object-or-string) establishes a mapping in mode from character to the function indicated by object"
#define Q_create_binding s7_make_signature(vv->s, 4, vv->sym_not, vv->sym_symbol_p, s7_make_signature(vv->s, 2, vv->sym_character_p, vv->sym_symbol_p), s7_make_signature(vv->s, 2, vv->sym_string_p, vv->sym_c_object_p))
	POP(mode, "LOW-create-binding", s7_is_symbol, "symbol");

	GPOP(ch_or_sp, "LOW-create-binding");
	if (!s7_is_character(ch_or_sp) && !s7_is_symbol(ch_or_sp)) {
		return s7_wrong_type_arg_error(s, "LOW-create-binding", _argument_number-1, ch_or_sp, "a character or a symbol");
	}

	GPOP(bj, "LOW-create-binding");
	Function *f = s7_to_v_function(vv, bj);
	if (!f) {
		return s7_wrong_type_arg_error(s, "LOW-create-binding", _argument_number-1, bj, "a c object with type function-function, function-transformation, or function-motion");
	}

	Keymap *km = NULL;
	if (s7_is_eq(mode, vv->sym_insert)) km = &vv->km_insert;
	else if (s7_is_eq(mode, vv->sym_motion)) km = &vv->km_motion;
	else if (s7_is_eq(mode, vv->sym_mutation)) km = &vv->km_mutate;
	else if (s7_is_eq(mode, vv->sym_function)) km = &vv->km_function;

	if (!km) return s7_wrong_type_arg_error(s, "LOW-create-binding", 1, mode, "a symbol: one of insert, motion, transform, or function)");

	Function **tgt = NULL;
	if (s7_is_character(ch_or_sp)) tgt = km->ascii + s7_character(ch_or_sp);
	else if (s7_is_eq(ch_or_sp, vv->sym_left)) tgt = km->special + SpecialKeyLeft;
	else if (s7_is_eq(ch_or_sp, vv->sym_right)) tgt = km->special + SpecialKeyRight;
	else if (s7_is_eq(ch_or_sp, vv->sym_up)) tgt = km->special + SpecialKeyUp;
	else if (s7_is_eq(ch_or_sp, vv->sym_down)) tgt = km->special + SpecialKeyDown;
	else if (s7_is_eq(ch_or_sp, vv->sym_enter)) tgt = km->special + SpecialKeyEnter;
	else if (s7_is_eq(ch_or_sp, vv->sym_backspace)) tgt = km->special + SpecialKeyBackspace;
	else if (s7_is_eq(ch_or_sp, vv->sym_delete)) tgt = km->special + SpecialKeyDelete;
	else if (s7_is_eq(ch_or_sp, vv->sym_escape)) tgt = km->special + SpecialKeyEscape;
	else if (s7_is_eq(ch_or_sp, vv->sym_tab)) tgt = km->special + SpecialKeyTab;

	if (!tgt) return s7_wrong_type_arg_error(s, "LOW-create-binding", 2, ch_or_sp, "a character or a symbol in (left right up down enter backspace delete escape tab)");
	*tgt = cnew(*f);

	return s7_f(s);
}

// symbol -> ()
static s7_pointer change_mode(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_change_mode "(LOW-change-mode mode) changes the current v's mode to the given one"
#define Q_change_mode s7_make_signature(vv->s, 2, vv->sym_not, vv->sym_symbol_p)
	POP(smode, "LOW-change-mode", s7_is_symbol, "symbol");
	Mode mode;
	if (!sym_to_mode(vv, smode, &mode)) {
		return s7_wrong_type_arg_error(s, "LOW-change-mode", 1, smode, "a symbol: one of insert, motion, mutation, or function)");
	}
	vv->v->mode = mode;
	return s7_f(s);
}

// () -> symbol
static s7_pointer current_mode(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_current_mode "(current-mode) returns the current v's mode"
#define Q_current_mode s7_make_signature(vv->s, 1, vv->sym_symbol_p)
	return mode_to_sym(vv->v->mode, vv);
}

static s7_pointer cursor_location(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_cursor_location "(cursor-location) returns returns the current cursor location"
#define Q_cursor_location s7_make_signature(vv->s, 1, vv->sym_c_pointer_p)
	return s7_make_c_object(s, vv->tag_loc, cnew(vv->v->b.loc));
}

static s7_pointer line_count(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_line_count "(line-count) returns the number of lines in the current buffer"
#define Q_line_count s7_make_signature(vv->s, 1, vv->sym_integer_p)
	return s7_make_integer(s, vv->v->b.tb.l);
}

static s7_pointer byte_count(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_byte_count "(byte-count line) returns the number of bytes in line 'line' of the current buffer"
#define Q_byte_count s7_make_signature(vv->s, 2, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "byte-count");

	return s7_make_integer(s, vv->v->b.tb.lines[line].bsz);
}

static s7_pointer grapheme_count(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_grapheme_count "(grapheme-count line) returns the number of graphemes in line 'line' of the current buffer"
#define Q_grapheme_count s7_make_signature(vv->s, 2, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "grapheme-count");

	return s7_make_integer(s, vv->v->b.tb.lines[line].gsz);
}

static s7_pointer text_remove(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_text_remove "(LOW-text-remove target) removes characters between the current cursor position and the x position indicated by the target"
#define Q_text_remove s7_make_signature(vv->s, 2, vv->sym_not, vv->sym_c_pointer_p)
	CPPOP(Loc, l, "LOW-text-remove", vv->tag_loc);
	b_remove(&vv->v->b, l->col);
	return s7_f(s);
}

static s7_pointer text_insert(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_text_insert "(LOW-text-insert loc text) inserts 'text' at the position indicated by 'loc'"
#define Q_text_insert s7_make_signature(vv->s, 2, vv->sym_not, vv->sym_c_pointer_p, vv->sym_string_p)
	CPPOP(Loc, l, "LOW-text-isnert", vv->tag_loc);
	POP(ps, "LOW-text-insert", s7_is_string, "string");
	tb_insert(&vv->v->b.tb, l->y, l->bx, (const u1*)s7_string(ps), (usz)s7_string_length(ps), &(usz){0});

	return s7_f(s);
}

static s7_pointer iterate(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_iterate "(iterate mode forward autosquish) created a new iterator in the current text buffer starting at the current cursor position according to the specified arguments"
#define Q_iterate s7_make_signature(vv->s, 4, vv->sym_c_pointer_p, vv->sym_boolean_p, vv->sym_boolean_p, vv->sym_boolean_p)
	POP(smode, "iterate", s7_is_symbol, "symbol");
	TbiMode mode;
	if (s7_is_eq(smode, vv->sym_stop_before_nl)) mode = TbiMode_StopBeforeNl;
	else if (s7_is_eq(smode, vv->sym_stop_after_nl)) mode = TbiMode_StopAfterNl;
	else if (s7_is_eq(smode, vv->sym_eat_everything)) mode = TbiMode_EatEverything;
	else return s7_wrong_type_arg_error(s, "iterate", 1, smode, "a symbol: one of stop-before-newline, stop-after-newline, or eat-everything");

	BPOP(forward, "iterate");
	BPOP(autosquish, "iterate");

	TextBufferIter *tbi = tb_iter(&vv->v->b.tb, vv->v->b.loc, mode, forward, autosquish);
	return s7_make_c_pointer_with_type(s, tbi, vv->sym_text_buffer_iter, s7_nil(s));
}

//todo vvv should be methods
static s7_pointer iterator_out(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_iterator_out "(iterator-out iterator) returns #t if the passed iterator will return no more"
#define Q_iterator_out s7_make_signature(vv->s, 2, vv->sym_boolean_p, vv->sym_c_pointer_p)
	POP(pi, "iterator-out", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pi, vv->sym_text_buffer_iter))) {
		return s7_wrong_type_arg_error(s, "LOW-text-insert", _argument_number, pi, "a c pointer with type text-buffer-iterator");
	}

	return s7_make_boolean(s, tbi_out(s7_c_pointer(pi)));
}
static s7_pointer iterator_loc(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_iterator_loc "(iterator-loc iterator) returns the current location of the passed iterator"
#define Q_iterator_loc s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_c_pointer_p)
	POP(pi, "iterator-loc", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pi, vv->sym_text_buffer_iter))) {
		return s7_wrong_type_arg_error(s, "LOW-text-insert", _argument_number, pi, "a c pointer with type text-buffer-iterator");
	}
	return s7_make_c_object(s, vv->tag_loc, cnew(tbi_cursor(s7_c_pointer(pi))));
}

static s7_pointer iterator_read(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_iterator_read "(iterator-read iterator advance) returns the current readation of the passed iterator"
#define Q_iterator_read s7_make_signature(vv->s, 3, vv->sym_string_p, vv->sym_c_pointer_p, vv->sym_boolean_p)
	POP(pi, "iterator-read", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pi, vv->sym_text_buffer_iter))) {
		return s7_wrong_type_arg_error(s, "LOW-text-insert", _argument_number, pi, "a c pointer with type text-buffer-iterator");
	}
	BPOP(advance, "iterator-read");

	const u1 *str;
	usz bsz, vsz;
	tbi_read(s7_c_pointer(pi), advance, &str, &bsz, &vsz);
	return s7_make_string_with_length(s, (const char*)str, bsz); //todo (values str vsz) or similar?
}

static s7_pointer create_cursor(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_create_cursor "(UNSAFE-create-cursor y gx bx) creates a cursor.  Parameters are not checked."
#define Q_create_cursor s7_make_signature(vv->s, 4, vv->sym_c_pointer_p, vv->sym_integer_p, vv->sym_integer_p, vv->sym_integer_p)
	UPOP(y, "create-cursor");
	UPOP(gx, "create-cursor");
	UPOP(bx, "create-cursor");
	return s7_make_c_object(s, vv->tag_loc, onew(Loc, .y=y, .gx=gx, .bx=bx));
}

static s7_pointer cursor_at(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_cursor_at "(cursor-at y gx) creates a cursor corresponding to the point before grapheme gx on line y of the current buffer.  Potentially O(n)"
#define Q_cursor_at s7_make_signature(vv->s, 3, vv->sym_c_pointer_p, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(y, vv->v->b.tb.l, "cursor-at");
	URPOP(gx, 1+vv->v->b.tb.lines[y].gsz, "cursor-at");
	return s7_make_c_object(s, vv->tag_loc, cnew(tb_cursor_at(&vv->v->b.tb, y, gx)));
}

static s7_pointer cursor_accessor(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
	CPPOP(Loc, l, NULL, vv->tag_loc);
	POP(it, "cursor", s7_is_symbol, "symbol");
	if (s7_is_eq(it, vv->sym_y)) return s7_make_integer(s, l->y);
	if (s7_is_eq(it, vv->sym_gx)) return s7_make_integer(s, l->gx);

	return s7_wrong_type_arg_error(s, "cursor", 2, it, "one of y or gx");
}

static s7_pointer function_motion_accessor(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
	CPPOP(Function, f, NULL, vv->tag_function_motion);
	if (!s7_is_null(s, args)) {
		return s7_wrong_number_of_args_error(s, "function-motion", _original_args);
	}
	assert(f->type.type == TypeMotion);
	return s7_make_c_object(s, vv->tag_loc, cnew(f->motion.perform(vv->v, f->motion.state)));
}

// 'LOW': low-level functions that may be inconvenient to use; you may prefer to use the higher-level wrappers defined by the boot code
// 'UNSAFE': unsafe functions that can create inconsistent state and whose use is prone to memory bugs
void vs7_init(VV *vv) {
#define TAG(n, sn) vv->tag_ ## n = s7_make_c_type(vv->s, sn)
#define ATAG(n, sn, f) TAG(n, sn); s7_c_type_set_ref(vv->s, vv->tag_ ## n, create_thunk(f, 1, 2, vv))
	ATAG(loc, "cursor", cursor_accessor);
	ATAG(function_motion, "function-motion", function_motion_accessor);
	TAG(function_function, "function-function");
	TAG(function_mutation, "function-mutation");
#undef ATAG
#undef TAG

	s7_pointer (*_v_thunk_typechecker)(s7_scheme*, s7_pointer, VV*);
#define FN(sn, cn, param) s7_define_typed_function(vv->s, sn, cn, param, param, false, H_ ## cn, Q_ ## cn)
#define VVTFN(sn, cn, param) s7_define_typed_function(vv->s, sn, create_thunk(_v_thunk_typechecker=cn, 1, 2, vv), param, param, false, H_ ## cn, Q_ ## cn)
	VVTFN("UNSAFE-create-cursor", create_cursor, 3);
	VVTFN("LOW-make-higher-order-function", make_higher_order_function, 3);
	VVTFN("LOW-make-motion", make_motion, 1);
	VVTFN("LOW-make-mutation", make_mutation, 3);
	VVTFN("LOW-create-binding", create_binding, 3);
	VVTFN("LOW-text-remove", text_remove, 1);
	VVTFN("LOW-text-insert", text_insert, 2);
	VVTFN("LOW-change-mode", change_mode, 1);
	VVTFN("current-mode", current_mode, 0);
	VVTFN("cursor-at", cursor_at, 2);
	VVTFN("cursor-location", cursor_location, 0);

	VVTFN("iterate", iterate, 3);
	VVTFN("iterator-out", iterator_out, 1);
	VVTFN("iterator-loc", iterator_loc, 1);
	VVTFN("iterator-read", iterator_read, 2);

	VVTFN("line-count", line_count, 0);
	VVTFN("byte-count", byte_count, 1);
	VVTFN("grapheme-count", grapheme_count, 1);

	VVTFN("set-highlighter", set_highlighter, 3);
#undef VVTFN
#undef FN

	s7_define_variable(vv->s, "___vv", s7_make_c_pointer(vv->s, vv)); //todo can we establish a side channel for this?

	s7_add_to_load_path(vv->s, "s");
	s7_load(vv->s, "prelude/boot.scm");
}

void vs7_deinit(VV *vv) {
	s7_free(vv->s);
	vv->s = NULL;
}
