#include "v.h"

bool type_compatible(Type *param, Type *arg) {
	if (arg->type == TypeBottom || param->type == TypeTop) return true;
	if (param->type != arg->type) return false; //todo motion can be treated as a type of transformation?
	if (param->type == TypeFunction) {
		return type_compatible(&param->fn[0], &arg->fn[0]) && type_compatible(&arg->fn[1], &param->fn[1]);
	}
	return true;
}

//todo actually push if f is hof and v->current
void v_push(V *v, Function *f) {
	if (!v->current) {
		v->current = new(Function, 1);
		*v->current = *f;
		return;
	}

	assert(v->current->type.type == TypeFunction);
	if (!type_compatible(&v->current->type.fn[0], &f->type)) {
		//todo push when f->type.fn[1] ≤ v->current->type.fn[0] (possibly recursively)
		msg(v, "type mismatch");
		return;
	}

	Function *n = new(Function, 1);
	*n = v->current->function.transform(v, v->current->function.state, f);
	v->current = n;
}

// returns true if something happened
bool v_reduce(V *v) {
	if (v->current && v->current->type.type != TypeFunction) {
		apply_transformation(v, v->current);
		v->current = NULL;
		msg(v, "");
		return true;
	}

	return false;
}
