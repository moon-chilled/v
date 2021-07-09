#include "v.h"

#define EMUT(name, fprepare, fperform, fundo) Function mutation_ ## name = {.type={TypeMutation}, .mutation={.prepare=fprepare, .perform=fperform, .undo=fundo}}
#define MUT(name) EMUT(name, prepare_ ## name, perform_ ## name, undo_ ## name)

static void prepare_stash_col(const V *v, void **state) {
	*state = Cnew(Col, v->b.loc.col);
}
static void perform_ins_nl(V *v, const void *state) {
        tb_insert_line(&v->b.tb, ++v->b.loc.y);
        usz bl = v->b.tb.lines[v->b.loc.y - 1].bsz - v->b.loc.bx;
	usz gl;
        tb_insert(&v->b.tb, v->b.loc.y, 0, v->b.tb.lines[v->b.loc.y - 1].chars + v->b.loc.bx, bl, &gl);
        tb_remove(&v->b.tb, v->b.loc.y - 1, v->b.loc.bx, bl, gl);
        v->b.loc.bx = v->b.loc.gx = 0;
}
static void undo_ins_nl(V *v, const void *state) {
	v->b.loc.y--;
	v->b.loc.bx = v->b.tb.lines[v->b.loc.y].bsz;
	v->b.loc.gx = v->b.tb.lines[v->b.loc.y].gsz;
	tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.bx, v->b.tb.lines[v->b.loc.y + 1].chars, v->b.tb.lines[v->b.loc.y + 1].bsz, &(usz){0});
	tb_remove_line(&v->b.tb, v->b.loc.y + 1);
}
EMUT(ins_nl, prepare_stash_col, perform_ins_nl, undo_ins_nl);

static void prepare_undo(const V *v, void **state) {
	*state = cnew((AppliedFunction)v->most_recent);
}
static void perform_undo(V *v, const void *state) {
	AppliedFunction f = *(AppliedFunction*)state;

	unapply_transformation(v, f);
}
// ;o
static void undo_undo(V *v, const void *state) {
	AppliedFunction f = *(AppliedFunction*)state;

	apply_transformation(v, &f.f);
}
MUT(undo);

#undef MUT
#undef EMUT
