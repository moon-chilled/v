#include "v.h"

static Loc motion_perform(const V *v, const void *state) {
	s7_pointer f = (s7_pointer)state; //const correctness, thou art a foul temptress!
	//s7_pointer the_v = s7_make_c_pointer_with_type(v->vv->s, v, v->vv->sym_v, s7_nil(v->vv->s));

	s7_pointer loc = s7_call(v->vv->s, f, s7_nil(v->vv->s));
	assert (s7_is_list(v->vv->s, loc) && s7_is_integer(s7_car(loc)) && s7_is_integer(s7_cdr(loc)));

	Loc r = v->b.loc;
	r.y = s7_integer(s7_car(loc));
	r.x = s7_integer(s7_cdr(loc));
	return r;
}

// function -> cpointer
static s7_pointer make_motion(s7_scheme *s, s7_pointer args) {
	assert (s7_is_list(s, args) && s7_is_null(s, s7_cdr(args)) && s7_is_procedure(s7_car(args)));
	s7_pointer f = s7_car(args);
	Function r = new_motion(f, motion_perform);
	return s7_make_c_pointer_with_type(s, cnew(r), s7_make_symbol(s, "function-motion"), s7_nil(s)); //todo reference vv->sym_function_motion
}

// character -> cpointer -> nil
static s7_pointer create_binding(s7_scheme *s, s7_pointer args) {
	VV *vv = s7_c_pointer(s7_name_to_value(s, "__vv"));
	assert (s7_is_list(s, args) && s7_is_character(s7_car(args)) && s7_is_list(s, s7_cdr(args)) && s7_is_null(s, s7_cddr(args)));
	u1 ch = s7_character(s7_car(args));
	s7_pointer bj = s7_cadr(args);
	assert (s7_is_c_pointer_of_type(bj, vv->sym_function_function) || s7_is_c_pointer_of_type(bj, vv->sym_function_transformation) || s7_is_c_pointer_of_type(bj, vv->sym_function_motion));
	Function *f = s7_c_pointer(bj);
	vv->km_motion/*TODO*/.ascii[ch] = cnew(*f);
	return s7_nil(s);
}

void vs7_init(VV *vv) {
	s7_define_safe_function(vv->s, "make-motion", make_motion, 1, 1, false, "TODO");
	s7_define_safe_function(vv->s, "create-binding", create_binding, 2, 2, false, "TODO");
	s7_define_variable(vv->s, "__vv", s7_make_c_pointer(vv->s, vv)); //todo can we establish a side channel for this?
}
