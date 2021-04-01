#include "v.h"

void v_push(V *v, Function *f) {
	if (!v->current) {
		v->current = new(Function, 1);
		*v->current = *f;
		return;
	}

	assert(v->current->type == FunctionHigherOrder);
	if (!(f->type & v->current->higher_order.parameter)) {
		msg(v, "type mismatch!");
		return;
	}

	Function *n = new(Function, 1);
	*n = v->current->higher_order.transform(v, v->current->higher_order.state, f);
	v->current = n;
}

// returns true if something happened
bool v_reduce(V *v) {
	if (v->current && v->current->type != FunctionHigherOrder) {
		apply_transformation(v, v->current);
		v->current = NULL;
		msg(v, "");
		return true;
	}

	return false;
}
