#include "v.h"

static VV *get_vv(s7_scheme *s) {
	return s7_c_pointer(s7_name_to_value(s, "___vv"));
}

struct s7_mutation { s7_pointer prepare, perform, undo; };

static Loc motion_perform(const V *v, const void *state) {
	s7_pointer f = (s7_pointer)state; //const correctness, thou art a foul temptress!

	s7_pointer loc = s7_call(v->vv->s, f, s7_nil(v->vv->s));
	if (!s7_is_c_pointer_of_type(loc, v->vv->sym_loc)) {
		msg((V*)v, "scheme error: object '%s' was unexpectedly not a location", s7_object_to_c_string(v->vv->s, loc));
		return v->b.loc;
	}

	return *(Loc*)s7_c_pointer(loc);
}

static void mutation_prepare(const V *v, void **state) {
	struct s7_mutation *m = *state;
	s7_call(v->vv->s, m->prepare, s7_nil(v->vv->s));
}
static void mutation_perform(V *v, const void *state) {
	const struct s7_mutation *m = state;
	s7_call(v->vv->s, m->perform, s7_nil(v->vv->s));
}
static void mutation_undo(V *v, const void *state) {
	const struct s7_mutation *m = state;
	s7_call(v->vv->s, m->undo, s7_nil(v->vv->s));
}

#define PRELUDE int __attribute__((unused)) _argument_number = 1; VV *vv = get_vv(s);
#define POP(x, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); s7_pointer x = s7_car(args); args = s7_cdr(args); _argument_number++
#define TPOP(T, x, get, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); T x = get(s7_car(args)); args = s7_cdr(args); _argument_number++
#define STPOP(T, x, get, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); T x = get(s, s7_car(args)); args = s7_cdr(args); _argument_number++
#define IPOP(x, fn) TPOP(s7_int, x, s7_integer, fn, s7_is_integer, "integer")
#define RPOP(x, min, max, fn) if (!s7_is_pair(args) || !s7_is_integer(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), "integer"); s7_int x = s7_integer(s7_car(args)); if (x < (s7_int)min) { char buf[128]; sprintf(buf, ">=%lli", (long long)min); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); }  if (x >= (s7_int)max) { char buf[128]; sprintf(buf, "<%lli", (long long)max); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); } args = s7_cdr(args)
#define URPOP(x, max, fn) RPOP(x ## _tmp, 0, max, fn); usz x = x ## _tmp
#define CPOP(x, fn) TPOP(u1, x, s7_character, fn, s7_is_character, "character")
#define PPOP(x, fn) POP(x, fn, s7_is_procedure, "procedure")
#define BPOP(x, fn) STPOP(bool, x, s7_boolean, fn, s7_is_boolean, "boolean")

//todo check aritability of functions

// function -> cpointer
static s7_pointer make_motion(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_make_motion "(LOW-make-motion fn) makes a motion out of fn"
#define Q_make_motion s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_procedure_p)
	PPOP(f, "LOW-make-motion");

	Function r = new_motion(f, motion_perform);
	return s7_make_c_pointer_with_type(s, cnew(r), vv->sym_function_motion, s7_nil(s));
}

// function -> function -> function -> cpointer
static s7_pointer make_mutation(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_make_mutation "(LOW-make-mutation prepare perform undo) makes a mutation out of its parameters"
#define Q_make_mutation s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_procedure_p, vv->sym_procedure_p, vv->sym_procedure_p)
	PPOP(prepare, "LOW-make-motion");
	PPOP(perform, "LOW-make-motion");
	PPOP(undo, "LOW-make-motion");

	Function r = new_mutation(onew(struct s7_mutation, .prepare=prepare, .perform=perform, .undo=undo), mutation_prepare, mutation_perform, mutation_undo);
	return s7_make_c_pointer_with_type(s, cnew(r), vv->sym_function_mutation, s7_nil(s));
}

// symbol (mode) -> character -> cpointer -> nil
static s7_pointer create_binding(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_create_binding "(LOW-create-binding mode character pointer) establishes a mapping in mode from character to the function indicated by pointer"
#define Q_create_binding s7_make_signature(vv->s, 4, vv->sym_not, vv->sym_symbol_p, vv->sym_character_p, vv->sym_c_pointer_p)
	POP(mode, "LOW-create-binding", s7_is_symbol, "symbol");

	CPOP(ch, "LOW-create-binding");

	POP(bj, "LOW-create-binding", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(bj, vv->sym_function_function) || s7_is_c_pointer_of_type(bj, vv->sym_function_mutation) || s7_is_c_pointer_of_type(bj, vv->sym_function_motion))) {
		return s7_wrong_type_arg_error(s, "LOW-create-binding", _argument_number, bj, "a c pointer with type function-function, function-transformation, or function-motion");
	}
	Function *f = s7_c_pointer(bj);

	Keymap *km = NULL;
	if (s7_is_eq(mode, vv->sym_insert)) km = &vv->km_insert;
	else if (s7_is_eq(mode, vv->sym_motion)) km = &vv->km_motion;
	else if (s7_is_eq(mode, vv->sym_mutation)) km = &vv->km_mutate;
	else if (s7_is_eq(mode, vv->sym_function)) km = &vv->km_function;

	if (!km) return s7_wrong_type_arg_error(s, "LOW-create-binding", 1, mode, "a symbol: one of insert, motion, transform, or function)");

	km->ascii[ch] = cnew(*f);

	return s7_f(s);
}

static s7_pointer cursor_location(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_cursor_location "(cursor-location) returns returns the current cursor location"
#define Q_cursor_location s7_make_signature(vv->s, 1, vv->sym_c_pointer_p)
	return s7_make_c_pointer_with_type(s, cnew(vv->v->b.loc), vv->sym_loc, s7_nil(s));
}

static s7_pointer line_count(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_line_count "(line-count) returns the number of lines in the current buffer"
#define Q_line_count s7_make_signature(vv->s, 1, vv->sym_integer_p)
	return s7_make_integer(s, vv->v->b.tb.l);
}

static s7_pointer byte_count(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_byte_count "(byte-count line) returns the number of bytes in line 'line' of the current buffer"
#define Q_byte_count s7_make_signature(vv->s, 2, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "byte-count");

	return s7_make_integer(s, vv->v->b.tb.lines[line].bsz);
}

static s7_pointer grapheme_count(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_grapheme_count "(grapheme-count line) returns the number of graphemes in line 'line' of the current buffer"
#define Q_grapheme_count s7_make_signature(vv->s, 2, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "grapheme-count");

	return s7_make_integer(s, vv->v->b.tb.lines[line].gsz);
}

#if 0
static s7_pointer character_at(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_character_at "(character-at y x) returns the character at location (y x) in the current buffer, potentially truncated"
#define Q_character_at s7_make_signature(vv->s, 3, vv->sym_character_p, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "character-at");
	URPOP(col, vv->v->b.tb.lines[line].l, "character-at");
	return s7_make_character(s, vv->v->b.tb.lines[line].glyphs[col]);
}
static s7_pointer codepoint_at(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_codepoint_at "(codepoint-at y x) returns the codepoint at location (y x) in the current buffer"
#define Q_codepoint_at s7_make_signature(vv->s, 3, vv->sym_integer_p, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "codepoint-at");
	URPOP(col, vv->v->b.tb.lines[line].l, "codepoint-at");
	return s7_make_integer(s, vv->v->b.tb.lines[line].glyphs[col]);
}
#endif

static s7_pointer text_remove(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_text_remove "(LOW-text-remove target) removes characters between the current cursor position and the x position indicated by the target"
#define Q_text_remove s7_make_signature(vv->s, 2, vv->sym_not, vv->sym_c_pointer_p)
	POP(pl, "LOW-create-binding", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pl, vv->sym_loc))) {
		return s7_wrong_type_arg_error(s, "LOW-create-binding", _argument_number, pl, "a c pointer with type loc");
	}
	Loc *l = s7_c_pointer(pl);


	b_remove(&vv->v->b, l->col);
	return s7_f(s);
}

static s7_pointer text_insert(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_text_insert "(LOW-text-insert loc text) inserts 'text' at the position indicated by 'loc'"
#define Q_text_insert s7_make_signature(vv->s, 2, vv->sym_not, vv->sym_c_pointer_p, vv->sym_string_p)
	POP(pl, "LOW-text-insert", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pl, vv->sym_loc))) {
		return s7_wrong_type_arg_error(s, "LOW-text-insert", _argument_number, pl, "a c pointer with type loc");
	}
	Loc *l = s7_c_pointer(pl);

	POP(ps, "LOW-text-insert", s7_is_string, "string");
	tb_insert(&vv->v->b.tb, l->y, l->bx, (const u1*)s7_string(ps), (usz)s7_string_length(ps), &(usz){0});

	return s7_f(s);
}

static s7_pointer iterate(s7_scheme *s, s7_pointer args) {PRELUDE
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
static s7_pointer iterator_out(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_iterator_out "(iterator-out iterator) returns #t if the passed iterator will return no more"
#define Q_iterator_out s7_make_signature(vv->s, 2, vv->sym_boolean_p, vv->sym_c_pointer_p)
	POP(pi, "iterator-out", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pi, vv->sym_text_buffer_iter))) {
		return s7_wrong_type_arg_error(s, "LOW-text-insert", _argument_number, pi, "a c pointer with type text-buffer-iterator");
	}

	return s7_make_boolean(s, tbi_out(s7_c_pointer(pi)));
}
static s7_pointer iterator_loc(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_iterator_loc "(iterator-loc iterator) returns the current location of the passed iterator"
#define Q_iterator_loc s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_c_pointer_p)
	POP(pi, "iterator-loc", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pi, vv->sym_text_buffer_iter))) {
		return s7_wrong_type_arg_error(s, "LOW-text-insert", _argument_number, pi, "a c pointer with type text-buffer-iterator");
	}
	Loc l = tbi_cursor(s7_c_pointer(pi));
	return s7_make_c_pointer_with_type(s, cnew(l), vv->sym_loc, s7_nil(s));
}

static s7_pointer iterator_read(s7_scheme *s, s7_pointer args) {PRELUDE
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


void vs7_init(VV *vv) {
#define FN(sn, cn, param) s7_define_typed_function(vv->s, sn, cn, param, param, false, H_ ## cn, Q_ ## cn)
	FN("LOW-make-motion", make_motion, 1);
	FN("LOW-make-mutation", make_mutation, 3);
	FN("LOW-create-binding", create_binding, 3);
	FN("LOW-text-remove", text_remove, 1);
	FN("LOW-text-insert", text_insert, 2);
	FN("cursor-location", cursor_location, 0);

	FN("iterate", iterate, 3);
	FN("iterator-out", iterator_out, 1);
	FN("iterator-loc", iterator_loc, 1);
	FN("iterator-read", iterator_read, 2);

	FN("line-count", line_count, 0);
	FN("byte-count", byte_count, 1);
	FN("grapheme-count", grapheme_count, 1);

	//FN("character-at", character_at, 2);
	//FN("codepoint-at", codepoint_at, 2);
	s7_define_variable(vv->s, "___vv", s7_make_c_pointer(vv->s, vv)); //todo can we establish a side channel for this?

	s7_add_to_load_path(vv->s, "s");
	s7_load(vv->s, "boot.scm");
}
