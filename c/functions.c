#include "v.h"

static void perform_delete(V *v, const void *state) {
	const Loc *dst = state;
#if 0
	if (dst->y < v->b.loc.y) {
		for (usz i = dst->y + 1; i < v->b.loc.y-1; i++) {
			tb_remove_line(&v->b.tb, dst->y + 1);
		}
		//todo can have 'tb_emplace'
		tb_remove(&v->b.tb, dst->y, dst->x, v->b.tb.lines[dst->y].l - dst->x);
		tb_insert(&v->b.tb, dst->y, dst->x, v->b.tb.lines[dst->y+1].glyphs + v->b.loc.x, v->b.tb.lines[dst->y+1].l - v->b.loc.x);
		tb_remove_line(&v->b.tb, dst->y + 1);
		v->b.loc = *dst;
		return;
	} else if (dst->y > v->b.loc.y) {
		assert(0); //todo
		return;
	}
#endif
	assert (dst->y == v->b.loc.y); //todo

	b_remove(&v->b, dst->col);
}
static void undo_delete(V *v, const void *state) { assert(0); /*todo*/ }
static Function deleter(const V *v, void *state, const Function *other) {
	assert (other->type.type == TypeMotion); //wg14 y u no HKT
	Loc *nloc = new(Loc, 1);
	*nloc = other->motion.perform(v, other->motion.state);
	return new_mutation(nloc, NULL, perform_delete, undo_delete);
	//todo pass other into prep
}

#define HOF(n, nmode, ret, parm, fun) static Type cv__ ## n ## __type[2] = {{.type=parm}, {.type=ret}}; Function hof_ ## n = {.type={.type=TypeFunction, .fn=cv__ ## n ## __type}, .function={.mode=nmode, .transform=fun}}
HOF(delete, ModeDefault, TypeMutation, TypeMotion, deleter);
#undef HOF

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

#if 0
static void perform_add_nl(V *v, const void *state) {
        tb_insert_line(&v->b.tb, ++v->b.loc.y);
        v->b.loc.x = 0;
}
static void undo_add_nl(V *v, const void *state) {
	tb_remove_line(&v->b.tb, v->b.loc.y--);
	v->b.loc.x = (usz)state;
}
EMUT(add_nl, prepare_stash_col, perform_add_nl, undo_add_nl);


static void perform_prep_nl(V *v, const void *state) {
	tb_insert_line(&v->b.tb, v->b.loc.y);
	v->b.loc.x = 0;
	v->mode = ModeInsert;
}
static void undo_prep_nl(V *v, const void *state) {
	undo_add_nl(v, state);
	v->mode = ModeNormal; //todo wrong
}
EMUT(prep_nl, prepare_stash_col, perform_prep_nl, undo_prep_nl);
#endif

#if 0
static void prepare_delback(const V *v, void **state) {
	if (v->b.loc.x) *state = (void*)(usz)v->b.tb.lines[v->b.loc.y].glyphs[v->b.loc.x-1];
}
static void perform_delback(V *v, const void *state) {
        if (v->b.loc.x) tb_remove(&v->b.tb, v->b.loc.y, --v->b.loc.x, 1);
}
static void undo_delback(V *v, const void *state) {
	if (v->b.loc.x) tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x++, &(glyph){(glyph)(usz)state}, 1);
}
MUT(delback);

static void prepare_delforward(const V *v, void **state) {
	if (v->b.loc.x < v->b.tb.lines[v->b.loc.y].l) *state = (void*)(usz)v->b.tb.lines[v->b.loc.y].glyphs[v->b.loc.x];
}
static void perform_delforward(V *v, const void *state) {
	if (v->b.loc.x < v->b.tb.lines[v->b.loc.y].l) tb_remove(&v->b.tb, v->b.loc.y, v->b.loc.x, 1);
}
static void undo_delforward(V *v, const void *state) {
	if (v->b.loc.x < v->b.tb.lines[v->b.loc.y].l) tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, &(glyph){(glyph)(usz)state}, 1);
}
MUT(delforward);
#endif

static void prepare_modeswitch(const V *v, void **state) {
	*state = (void*)(usz)v->mode;
}
static void undo_modeswitch(V *v, const void *state) {
	v->mode = (Mode)(usz)state;
}
static void perform_insert(V *v, const void *state) { v->mode = ModeInsert; }
EMUT(insert, prepare_modeswitch, perform_insert, undo_modeswitch);
static void perform_normal(V *v, const void *state) { v->mode = ModeNormal; }
EMUT(normal, prepare_modeswitch, perform_normal, undo_modeswitch);

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

#if 0
struct if_state { Mode old_mode; usz old_x; };
static void prepare_if_state(const V *v, void **state) {
	*state = onew(struct if_state, .old_mode=v->mode, .old_x=v->b.loc.x);
}
static void undo_if(V *v, const void *state) { const struct if_state *s = state; v->b.loc.x = s->old_x; v->mode = s->old_mode; }
static void perform_insert_front(V *v, const void *state) { v->b.loc.x = 0; v->mode = ModeInsert; }
static void perform_insert_back(V *v, const void *state) { v->b.loc.x = v->b.tb.lines[v->b.loc.y].l; v->mode = ModeInsert; }
EMUT(insert_front, prepare_if_state, perform_insert_front, undo_if);
EMUT(insert_back, prepare_if_state, perform_insert_back, undo_if);
#endif

#undef MUT
#undef EMUT
