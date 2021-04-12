#include "v.h"

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

#define PRELUDE int __attribute__((unused)) _argument_number = 1;
#define POP(x, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); s7_pointer x = s7_car(args); args = s7_cdr(args); _argument_number++
#define TPOP(T, x, get, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); T x = get(s7_car(args)); args = s7_cdr(args); _argument_number++
#define STPOP(T, x, get, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); T x = get(s, s7_car(args)); args = s7_cdr(args); _argument_number++
#define IPOP(x, fn) TPOP(s7_int, x, s7_integer, fn, s7_is_integer, "integer")
#define RPOP(x, min, max, fn) if (!s7_is_pair(args) || !s7_is_integer(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), "integer"); s7_int x = s7_integer(s7_car(args)); if (x < (s7_int)(min)) { char buf[128]; sprintf(buf, ">=%lli", (long long)(min)); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); }  if (x >= (s7_int)(max)) { char buf[128]; sprintf(buf, "<%lli", (long long)(max)); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); } args = s7_cdr(args)
#define URPOP(x, max, fn) RPOP(x ## _tmp, 0, max, fn); usz x = x ## _tmp
#define UPOP(x, fn) URPOP(x, 1ll << 62, fn)
#define CPOP(x, fn) TPOP(u1, x, s7_character, fn, s7_is_character, "character")
#define PPOP(x, fn) POP(x, fn, s7_is_procedure, "procedure")
#define BPOP(x, fn) STPOP(bool, x, s7_boolean, fn, s7_is_boolean, "boolean")

//todo check aritability of functions

// function -> cpointer
static s7_pointer make_motion(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_make_motion "(LOW-make-motion fn) makes a motion out of fn"
#define Q_make_motion s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_procedure_p)
	PPOP(f, "LOW-make-motion");

	Function r = new_motion(f, motion_perform);
	return s7_make_c_pointer_with_type(s, cnew(r), vv->sym_function_motion, s7_nil(s));
}

// function -> function -> function -> cpointer
static s7_pointer make_mutation(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_make_mutation "(LOW-make-mutation prepare perform undo) makes a mutation out of its parameters"
#define Q_make_mutation s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_procedure_p, vv->sym_procedure_p, vv->sym_procedure_p)
	PPOP(prepare, "LOW-make-motion");
	PPOP(perform, "LOW-make-motion");
	PPOP(undo, "LOW-make-motion");

	Function r = new_mutation(onew(struct s7_mutation, .prepare=prepare, .perform=perform, .undo=undo), mutation_prepare, mutation_perform, mutation_undo);
	return s7_make_c_pointer_with_type(s, cnew(r), vv->sym_function_mutation, s7_nil(s));
}

// symbol (mode) -> character -> cpointer -> nil
static s7_pointer create_binding(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
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

static s7_pointer cursor_location(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_cursor_location "(cursor-location) returns returns the current cursor location"
#define Q_cursor_location s7_make_signature(vv->s, 1, vv->sym_c_pointer_p)
	return s7_make_c_pointer_with_type(s, cnew(vv->v->b.loc), vv->sym_loc, s7_nil(s));
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
	POP(pl, "LOW-create-binding", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pl, vv->sym_loc))) {
		return s7_wrong_type_arg_error(s, "LOW-create-binding", _argument_number, pl, "a c pointer with type loc");
	}
	Loc *l = s7_c_pointer(pl);


	b_remove(&vv->v->b, l->col);
	return s7_f(s);
}

static s7_pointer text_insert(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
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
	return s7_make_c_pointer_with_type(s, cnew(tbi_cursor(s7_c_pointer(pi))), vv->sym_loc, s7_nil(s));
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

static s7_pointer create_cursor(s7_scheme *s, s7_pointer args, VV *vv) {int _argument_number = 1;
#define H_create_cursor "(UNSAFE-create-cursor y gx bx) creates a cursor.  Parameters are not checked."
#define Q_create_cursor s7_make_signature(vv->s, 4, vv->sym_c_pointer_p, vv->sym_integer_p, vv->sym_integer_p, vv->sym_integer_p)
	UPOP(y, "create-cursor");
	UPOP(gx, "create-cursor");
	UPOP(bx, "create-cursor");
	return s7_make_c_pointer_with_type(s, onew(Loc, .y=y, .gx=gx, .bx=bx), vv->sym_loc, s7_nil(s));
}

static s7_pointer cursor_at(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_cursor_at "(cursor-at y gx) creates a cursor corresponding to the point before grapheme gx on line y of the current buffer.  Potentially O(n)"
#define Q_cursor_at s7_make_signature(vv->s, 3, vv->sym_c_pointer_p, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(y, vv->v->b.tb.l, "cursor-at");
	URPOP(gx, 1+vv->v->b.tb.lines[y].gsz, "cursor-at");
	return s7_make_c_pointer_with_type(s, cnew(tb_cursor_at(&vv->v->b.tb, y, gx)), vv->sym_loc, s7_nil(s));
}

//todo
static s7_pointer cursor_y(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_cursor_y ""
#define Q_cursor_y s7_make_signature(vv->s, 0)
	POP(pl, "cursor-y", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pl, vv->sym_loc))) {
		return s7_wrong_type_arg_error(s, "cursor-gx", _argument_number, pl, "a c pointer with type loc");
	}
	Loc *l = s7_c_pointer(pl);
	return s7_make_integer(s, l->y);
}
static s7_pointer cursor_gx(s7_scheme *s, s7_pointer args, VV *vv) {PRELUDE
#define H_cursor_gx ""
#define Q_cursor_gx s7_make_signature(vv->s, 0)
	POP(pl, "cursor-gx", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(pl, vv->sym_loc))) {
		return s7_wrong_type_arg_error(s, "cursor-gx", _argument_number, pl, "a c pointer with type loc");
	}
	Loc *l = s7_c_pointer(pl);
	return s7_make_integer(s, l->gx);
}

// 'LOW': low-level functions that may be inconvenient to use; you may prefer to use the higher-level wrappers defined by the boot code
// 'UNSAFE': unsafe functions that can create inconsistent state and whose use is prone to memory bugs
void vs7_init(VV *vv) {
	s7_pointer (*_v_thunk_typechecker)(s7_scheme*, s7_pointer, VV*);
#define FN(sn, cn, param) s7_define_typed_function(vv->s, sn, cn, param, param, false, H_ ## cn, Q_ ## cn)
#define VVTFN(sn, cn, param) s7_define_typed_function(vv->s, sn, create_thunk(_v_thunk_typechecker=cn, 1, 2, vv), param, param, false, H_ ## cn, Q_ ## cn)
	VVTFN("UNSAFE-create-cursor", create_cursor, 3);
	VVTFN("LOW-make-motion", make_motion, 1);
	VVTFN("LOW-make-mutation", make_mutation, 3);
	VVTFN("LOW-create-binding", create_binding, 3);
	VVTFN("LOW-text-remove", text_remove, 1);
	VVTFN("LOW-text-insert", text_insert, 2);
	VVTFN("cursor-at", cursor_at, 2);
	VVTFN("cursor-location", cursor_location, 0);

	VVTFN("cursor-y", cursor_y, 1);
	VVTFN("cursor-gx", cursor_gx, 1);

	VVTFN("iterate", iterate, 3);
	VVTFN("iterator-out", iterator_out, 1);
	VVTFN("iterator-loc", iterator_loc, 1);
	VVTFN("iterator-read", iterator_read, 2);

	VVTFN("line-count", line_count, 0);
	VVTFN("byte-count", byte_count, 1);
	VVTFN("grapheme-count", grapheme_count, 1);
#undef VVTFN
#undef FN

	s7_define_variable(vv->s, "___vv", s7_make_c_pointer(vv->s, vv)); //todo can we establish a side channel for this?

	s7_add_to_load_path(vv->s, "s");
	s7_load(vv->s, "boot.scm");
}

void vs7_deinit(VV *vv) {
	s7_free(vv->s);
	vv->s = NULL;
}
