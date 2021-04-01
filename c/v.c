#include "v.h"

usz fmt_typetype(char *buf, TypeType type) {
	if (!type) {
		strcpy(buf, "bottom");
		return 6;
	}

	const char *pipe = "|";
	char *n = buf;
	if (type & TypeStr) n += sprintf(n, "%sstr", (pipe += !!*pipe)-1);
	if (type & TypeChar) n += sprintf(n, "%schar", (pipe += !!*pipe)-1);
	if (type & TypeMotion) n += sprintf(n, "%smotion", (pipe += !!*pipe)-1);
	if (type & TypeTransform) n += sprintf(n, "%stransform", (pipe += !!*pipe)-1);

	return n - buf;
}

usz fmt_type(char *buf, Type type) {
	char *n = buf;

	for (usz i = 0; i < type.arity; i++) {
		*n++ = '(';
		n += fmt_type(n, type.param[i]);
		*n++ = ')';
		*n++ = ' ';
		*n++ = '-';
		*n++ = '>';
		*n++ = ' ';
	}

	n += fmt_typetype(n, type.ret);

	return n - buf;
}

void v_push(V *v, Function *f) {
	if (!v->current) {
		v->current = new(Function, 1);
		*v->current = *f;
		return;
	}

	assert(v->current->type.arity);
	assert(v->current->type.arity == 1); //todo
	assert(!v->current->type.param[0].arity); //todo
	assert(!f->type.arity); //todo
	if (!(f->type.ret & v->current->type.param[0].ret)) {
		char wanted[4096], got[4096];
		fmt_typetype(got, f->type.ret);
		fmt_typetype(wanted, v->current->type.param[0].ret);
		msg(v, "type mismatch (expected %s, got %s)", wanted, got);
		return;
	}

	Function *n = new(Function, 1);
	*n = v->current->higher_order.transform(v, v->current->higher_order.state, f);
	v->current = n;
}

// returns true if something happened
bool v_reduce(V *v) {
	if (v->current && !v->current->type.arity) {
		apply_transformation(v, v->current);
		v->current = NULL;
		msg(v, "");
		return true;
	}

	return false;
}
