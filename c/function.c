#include "v.h"

void apply_transformation(V *v, const Function *f) {
	switch (f->type) {
		case FunctionStr: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, f->str.s, f->str.l); v->b.loc.x += f->str.l; break;
		case FunctionChar: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x++, &f->character, 1); break;
		case FunctionMotion: v->b.loc = f->motion(v); break;
		case FunctionTransform: f->action.perform(v, f->action.state); break;
		default: assert(0);
	}
}
