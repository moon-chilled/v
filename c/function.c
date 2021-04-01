#include "functions.h"

void apply_transformation(Buffer *b, const Function *f) {
	switch (f->type) {
		case FunctionStr: tb_insert(&b->tb, b->loc.y, b->loc.x, f->str.s, f->str.l); b->loc.x += f->str.l; break;
		case FunctionChar: tb_insert(&b->tb, b->loc.y, b->loc.x++, &f->character, 1); break;
		case FunctionMotion: b->loc = f->motion(b); break;
		case FunctionTransform: f->action.perform(b, f->action.state); break;
		default: assert(0);
	}
}
