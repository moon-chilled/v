#include "v.h"

AppliedFunction apply_transformation(V *v, Function *f) {
	AppliedFunction ret = {.floc = v->b.loc};
	switch (f->type.type) {
		case TypeStr: b_insert(&v->b, f->str.s, f->str.l); break;
		case TypeMotion: v->b.loc = f->motion.perform(v, f->motion.state); break;
		case TypeMutation: if (f->mutation.prepare) f->mutation.prepare(v, &f->mutation.state); f->mutation.perform(v, f->mutation.state); break;
		case TypeFunction: assert(0); break;
		default: assert(0);
	}
	ret.f = *f;
	ret.cloc = v->b.loc;
	return ret;
}
void unapply_transformation(V *v, AppliedFunction f) {
	switch (f.f.type.type) {
		case TypeStr:
			v->b.loc = f.floc; b_remove(&v->b, f.cloc.col); break;
		case TypeMotion: break;
		case TypeMutation: f.f.mutation.undo(v, f.f.mutation.state); break;
		case TypeFunction: assert(0); break;
		default: assert(0);
	}
	v->b.loc = f.floc;
}
