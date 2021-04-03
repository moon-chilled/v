#include "v.h"

bool type_compatible(const Type *param, const Type *arg) {
	if (arg->type == TypeBottom || param->type == TypeTop) return true;
	if (param->type != arg->type) return false; //todo motion can be treated as a type of transformation?
	if (param->type == TypeFunction) {
		return type_compatible(&param->fn[0], &arg->fn[0]) && type_compatible(&arg->fn[1], &param->fn[1]);
	}
	return true;
}

bool can_push(const Type *tos, const Type *prospective) {
	return type_compatible(tos, prospective)
	    || (prospective->type == TypeFunction && can_push(tos, &prospective->fn[1]));
}

void v_push(V *v, Function *f) {
	assert(!v->sp || v->stack[v->sp-1].f.type.type == TypeFunction);
	if (v->sp && !can_push(&v->stack[v->sp-1].f.type.fn[0], &f->type)) {
		msg(v, "type mismatch");
		return;
	}

	v->stack = GC_realloc(v->stack, sizeof(*v->stack) * ++v->sp);
	v->stack[v->sp-1].f = *f;
	v->stack[v->sp-1].old_mode = v->mode;
	if (f->type.type == TypeFunction && f->function.mode != ModeDefault) v->mode = f->function.mode;
}

// returns true if something happened
bool v_reduce(V *v) {
	bool ret = false;

	while (v->sp >= 2 && type_compatible(&v->stack[v->sp-2].f.type.fn[0], &v->stack[v->sp-1].f.type)) {
		ret = true;

		v->mode = v->stack[v->sp-1].old_mode; // useless?
		Function f = v->stack[v->sp-2].f.function.transform(v, v->stack[v->sp-2].f.function.state, &v->stack[v->sp-1].f);
		v->mode = v->stack[v->sp-2].old_mode;
		v->sp -= 2;

		v_push(v, &f);
	}

	if (v->sp && v->stack[v->sp-1].f.type.type != TypeFunction) {
		ret = true;

		apply_transformation(v, &v->stack[--v->sp].f);
	}

	return ret;
}
