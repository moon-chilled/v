#include "v.h"

void apply_transformation(V *v, const Function *f) {
	switch (f->type.type) {
		case TypeStr: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, f->str.s, f->str.l); v->b.loc.x += f->str.l; break;
		case TypeChar: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x++, &f->character, 1); break;
		case TypeMotion: v->b.loc = f->motion.perform(v, f->motion.state); break;
		case TypeTransform: f->action.perform(v, f->action.state); break;
		case TypeFunction: assert(0); break;
		default: assert(0);
	}
}
