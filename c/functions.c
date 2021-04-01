#include "v.h"

// ctype isspace is bad
static bool is_space(glyph g) {
	return g == ' ' || g == '\t' || g == '\n' || g == '\r' || g == '\f';
}

static Loc move_cleft(const V *v) {
        Loc r = v->b.loc;
        if (r.x) r.x--;
        return r;
}
static Loc move_cright(const V *v) {
        Loc r = v->b.loc;
        if (r.x + (v->mode == ModeNormal) < v->b.tb.lines[r.y].l) r.x++;
        return r;
}
static Loc move_cdown(const V *v) {
        Loc r = v->b.loc;
        if (r.y+1 < v->b.tb.l) {
                r.y++;
                r.x = min(r.x, v->b.tb.lines[/*++*/r.y].l - (v->mode == ModeNormal)); //wg14 y u no ({})
        }
        return r;
}
static Loc move_cup(const V *v) {
        Loc r = v->b.loc;
        if (r.y) {
                r.y--;
                r.x = min(r.x, v->b.tb.lines[r.y].l - (v->mode == ModeNormal));
        }
        return r;
}
static Loc move_eol(const V *v) {
	Loc r = v->b.loc;
	r.x = v->b.tb.lines[r.y].l - (v->mode == ModeNormal);
	return r;
}
static Loc move_bol(const V *v) {
	Loc r = v->b.loc;
	r.x = 0;
	return r;
}
static Loc move_wordforward(const V *v) {
	Loc r = v->b.loc;
	TextBuffer tb = v->b.tb;
	while (r.x + (v->mode == ModeNormal) < tb.lines[r.y].l && !is_space(tb.lines[r.y].glyphs[r.x])) {
		r.x++;
	}
	while (r.x + (v->mode == ModeNormal) < tb.lines[r.y].l && is_space(tb.lines[r.y].glyphs[r.x])) {
		r.x++;
	}
	return r;
}
static Loc move_wordback(const V *v) {
	Loc r = v->b.loc;
	TextBuffer tb = v->b.tb;
	while (r.x && is_space(tb.lines[r.y].glyphs[r.x-1])) {
		r.x--;
	}
	while (r.x && !is_space(tb.lines[r.y].glyphs[r.x-1])) {
		r.x--;
	}
	return r;
}
Function motion_cleft(const V *v) { return new_motion(move_cleft); }
Function motion_cright(const V *v) { return new_motion(move_cright); }
Function motion_cup(const V *v) { return new_motion(move_cup); }
Function motion_cdown(const V *v) { return new_motion(move_cdown); }
Function motion_eol(const V *v) { return new_motion(move_eol); }
Function motion_bol(const V *v) { return new_motion(move_bol); }
Function motion_wordforward(const V *v) { return new_motion(move_wordforward); }
Function motion_wordback(const V *v) { return new_motion(move_wordback); }

static void perform_ins_nl(V *v, void *state) {
        tb_insert_line(&v->b.tb, ++v->b.loc.y);
        v->b.loc.x = 0;
}
static void undo_ins_nl(V *v, void *state) {
	tb_remove_line(&v->b.tb, v->b.loc.y--);
	v->b.loc.x = (usz)state;
}
Function transform_ins_nl(const V *v) {
	return new_transformation((void*)v->b.loc.x, perform_ins_nl, undo_ins_nl);
}

static void perform_add_nl(V *v, void *state) {
        tb_insert_line(&v->b.tb, ++v->b.loc.y);
        v->b.loc.x = 0;
}
static void undo_add_nl(V *v, void *state) {
	tb_remove_line(&v->b.tb, v->b.loc.y--);
	v->b.loc.x = (usz)state;
}
Function transform_add_nl(const V *v) {
	return new_transformation((void*)v->b.loc.x, perform_add_nl, undo_add_nl);
}

static void perform_nothing(V *v, void *state) {}
static void undo_nothing(V *v, void *state) {}

static void perform_delback(V *v, void *state) {
        tb_remove(&v->b.tb, v->b.loc.y, --v->b.loc.x, 1);
}
static void undo_delback(V *v, void *state) {
	tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x++, &(glyph){(glyph)(usz)state}, 1);
}
Function transform_delback(const V *v) {
	if (!v->b.loc.x) return new_transformation(NULL, perform_nothing, undo_nothing);
	return new_transformation((void*)(usz)v->b.tb.lines[v->b.loc.y].glyphs[v->b.loc.x-1], perform_delback, undo_delback);
}

static void perform_delforward(V *v, void *state) {
	tb_remove(&v->b.tb, v->b.loc.y, v->b.loc.x, 1);
}
static void undo_delforward(V *v, void *state) {
	tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, &(glyph){(glyph)(usz)state}, 1);
}
Function transform_delforward(const V *v) {
	if (v->b.loc.x >= v->b.tb.lines[v->b.loc.y].l) return new_transformation(NULL, perform_nothing, undo_nothing);
	return new_transformation((void*)(usz)v->b.tb.lines[v->b.loc.y].glyphs[v->b.loc.x-1], perform_delforward, undo_delforward);
}

static void undo_modeswitch(V *v, void *state) {
	v->mode = (Mode)(usz)state;
}

static void perform_insert(V *v, void *state) { v->mode = ModeInsert; }
static void perform_normal(V *v, void *state) {
	v->mode = ModeNormal;
	v->b.loc.x = min(v->b.loc.x+1, v->b.tb.lines[v->b.loc.y].l);
	if (v->b.loc.x) v->b.loc.x--;
}
Function transform_insert(const V *v) { return new_transformation((void*)(usz)v->mode, perform_insert, undo_modeswitch); }
Function transform_normal(const V *v) { return new_transformation((void*)(usz)v->mode, perform_normal, undo_modeswitch); }





static void perform_delete(V *v, void *state) {
	Loc *dst = state;
	assert (dst->y == v->b.loc.y);
	if (dst->x < v->b.loc.x) {
		tb_remove(&v->b.tb, dst->y, dst->x, v->b.loc.x - dst->x);
		v->b.loc.x = dst->x;
	} else {
		tb_remove(&v->b.tb, dst->y, v->b.loc.x, dst->x - v->b.loc.x);
	}
}
static void undo_delete(V *v, void *state) { assert(0); /*todo*/ }


static Function deleter(V *v, void *state, const Function *other) {
	assert (other->type == FunctionMotion); //c: what are HKT?
	Loc *nloc = new(Loc, 1);
	*nloc = other->motion(v);
	return new_transformation(nloc, perform_delete, undo_delete);
}
Function hof_delete(const V *v) {
	return new_hof(NULL, FunctionMotion, deleter);
}
