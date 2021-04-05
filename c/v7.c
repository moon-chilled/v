#include "v.h"

static VV *get_vv(s7_scheme *s) {
	return s7_c_pointer(s7_name_to_value(s, "___vv"));
}

static Loc motion_perform(const V *v, const void *state) {
	s7_pointer f = (s7_pointer)state; //const correctness, thou art a foul temptress!

	s7_pointer loc = s7_call(v->vv->s, f, s7_nil(v->vv->s));
	if (!s7_is_list(v->vv->s, loc) && !s7_is_integer(s7_car(loc)) && !s7_is_integer(s7_cdr(loc))) {
		msg((V*)v, "scheme error: object '%s' was unexpectedly not a location", s7_object_to_c_string(v->vv->s, loc));
		return v->b.loc;
	}

	Loc r = v->b.loc;
	r.y = s7_integer(s7_car(loc));
	r.x = s7_integer(s7_cdr(loc));
	return r;
}

#define PRELUDE int __attribute__((unused)) _argument_number = 1; VV *vv = get_vv(s);
#define POP(x, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); s7_pointer x = s7_car(args); args = s7_cdr(args); _argument_number++
#define TPOP(T, x, get, fn, t_, t) if (!s7_is_pair(args) || !t_(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), t); T x = get(s7_car(args)); args = s7_cdr(args); _argument_number++
#define IPOP(x, fn) TPOP(s7_int, x, s7_integer, fn, s7_is_integer, "integer")
#define RPOP(x, min, max, fn) if (!s7_is_pair(args) || !s7_is_integer(s7_car(args))) return s7_wrong_type_arg_error(s, fn, _argument_number, s7_car(args), "integer"); s7_int x = s7_integer(s7_car(args)); if (x < (s7_int)min) { char buf[128]; sprintf(buf, ">=%lli", (long long)min); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); }  if (x >= (s7_int)max) { char buf[128]; sprintf(buf, "<%lli", (long long)max); return s7_out_of_range_error(s, fn, _argument_number, s7_car(args), buf); } args = s7_cdr(args)
#define URPOP(x, max, fn) RPOP(x ## _tmp, 0, max, fn); usz x = x ## _tmp
#define CPOP(x, fn) TPOP(u1, x, s7_character, fn, s7_is_character, "character")
#define PPOP(x, fn) POP(x, fn, s7_is_procedure, "procedure")

// function -> cpointer
static s7_pointer make_motion(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_make_motion "(LOW-make-motion fn) makes a motion out of fn"
#define Q_make_motion s7_make_signature(vv->s, 2, vv->sym_c_pointer_p, vv->sym_procedure_p)
	PPOP(f, "LOW-make-motion");

	Function r = new_motion(f, motion_perform);
	return s7_make_c_pointer_with_type(s, cnew(r), vv->sym_function_motion, s7_nil(s)); //todo reference vv->sym_function_motion
}

// symbol (mode) -> character -> cpointer -> nil
static s7_pointer create_binding(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_create_binding "(LOW-create-binding mode character pointer) establishes a mapping in mode from character to the function indicated by pointer"
#define Q_create_binding s7_make_signature(vv->s, 4, vv->sym_not, vv->sym_symbol_p, vv->sym_character_p, vv->sym_c_pointer_p)
	POP(mode, "LOW-create-binding", s7_is_symbol, "symbol");

	CPOP(ch, "LOW-create-binding");

	POP(bj, "LOW-create-binding", s7_is_c_pointer, "c pointer");
	if (!(s7_is_c_pointer_of_type(bj, vv->sym_function_function) || s7_is_c_pointer_of_type(bj, vv->sym_function_transformation) || s7_is_c_pointer_of_type(bj, vv->sym_function_motion))) {
		return s7_wrong_type_arg_error(s, "LOW-create-binding", _argument_number, bj, "a c pointer with type function-function, function-transformation, or function-motion");
	}
	Function *f = s7_c_pointer(bj);

	Keymap *km = NULL;
	if (s7_is_eq(mode, vv->sym_insert)) km = &vv->km_insert;
	else if (s7_is_eq(mode, vv->sym_motion)) km = &vv->km_motion;
	else if (s7_is_eq(mode, vv->sym_transform)) km = &vv->km_transform;
	else if (s7_is_eq(mode, vv->sym_function)) km = &vv->km_function;

	if (!km) return s7_wrong_type_arg_error(s, "LOW-create-binding", 1, mode, "a symbol: one of insert, motion, transform, or function)");

	km->ascii[ch] = cnew(*f);

	return s7_nil(s);
}

static s7_pointer cursor_location(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_cursor_location "(cursor-location) returns returns a cons (y . x) of the current cursor location"
#define Q_cursor_location s7_make_signature(vv->s, 1, vv->sym_pair_p)
	return s7_cons(s, s7_make_integer(s, vv->v->b.loc.y), s7_make_integer(s, vv->v->b.loc.x));
}

static s7_pointer line_count(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_line_count "(line-count) returns the number of lines in the current buffer"
#define Q_line_count s7_make_signature(vv->s, 1, vv->sym_integer_p)
	return s7_make_integer(s, vv->v->b.tb.l);
}

static s7_pointer character_count(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_character_count "(character-count line) returns the number of characters in line 'line' of the current buffer"
#define Q_character_count s7_make_signature(vv->s, 2, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "character-count");

	return s7_make_integer(s, vv->v->b.tb.lines[line].l);
}

static s7_pointer character_at(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_character_at "(character-at y x) returns the character at location (y, x) in the current buffer, potentially truncated"
#define Q_character_at s7_make_signature(vv->s, 3, vv->sym_character_p, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "character-at");
	URPOP(col, vv->v->b.tb.lines[line].l, "character-at");
	return s7_make_character(s, vv->v->b.tb.lines[line].glyphs[col]);
}
static s7_pointer codepoint_at(s7_scheme *s, s7_pointer args) {PRELUDE
#define H_codepoint_at "(codepoint-at y x) returns the codepoint at location (y, x) in the current buffer"
#define Q_codepoint_at s7_make_signature(vv->s, 3, vv->sym_integer_p, vv->sym_integer_p, vv->sym_integer_p)
	URPOP(line, vv->v->b.tb.l, "codepoint-at");
	URPOP(col, vv->v->b.tb.lines[line].l, "codepoint-at");
	return s7_make_integer(s, vv->v->b.tb.lines[line].glyphs[col]);
}

void vs7_init(VV *vv) {
#define FN(sn, cn, param) s7_define_typed_function(vv->s, sn, cn, param, param, false, H_ ## cn, Q_ ## cn)
	FN("LOW-make-motion", make_motion, 1);
	FN("LOW-create-binding", create_binding, 3);
	FN("cursor-location", cursor_location, 0);
	FN("line-count", line_count, 0);
	FN("character-count", character_count, 1);
	FN("character-at", character_at, 2);
	FN("codepoint-at", codepoint_at, 2);
	s7_define_variable(vv->s, "___vv", s7_make_c_pointer(vv->s, vv)); //todo can we establish a side channel for this?

	s7_add_to_load_path(vv->s, "s");
	s7_load(vv->s, "boot.scm");
}
