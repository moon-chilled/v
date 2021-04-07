#include "v.h"

void apply_transformation(V *v, Function *f) {
	switch (f->type.type) {
		case TypeStr: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, f->str.s, f->str.l); v->b.loc.x += f->str.l; break;
		case TypeChar: tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x++, &f->character, 1); break;
		case TypeMotion: v->b.loc = f->motion.perform(v, f->motion.state); break;
		case TypeMutation: if (f->mutation.prepare) f->mutation.prepare(v, &f->mutation.state); f->mutation.perform(v, f->mutation.state); break;
		case TypeFunction: assert(0); break;
		default: assert(0);
	}
}
