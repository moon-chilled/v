#include "v.h"

//todo actually push if f is hof and v->current
void v_push(V *v, Function *f) {
	if (!v->current) {
		v->current = new(Function, 1);
		*v->current = *f;
		return;
	}

	assert(v->current->type.type == TypeFunction);
	assert(v->current->type.fn[1].type != TypeFunction); //todo
	assert(v->current->type.fn[0].type != TypeFunction); //todo
	assert(f->type.type != TypeFunction); //todo
	if (f->type.type != v->current->type.fn[0].type) {
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
