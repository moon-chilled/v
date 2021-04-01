#include "v.h"

void apply_transformation(V *v, const Function *f) {
	assert (!f->type.arity);
	switch (f->type.ret) {
		case TypeStr: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, f->str.s, f->str.l); v->b.loc.x += f->str.l; break;
		case TypeChar: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x++, &f->character, 1); break;
		case TypeMotion: v->b.loc = f->motion(v); break;
		case TypeTransform: f->action.perform(v, f->action.state); break;
		default: assert(0);
	}
}
