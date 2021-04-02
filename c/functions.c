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
        if (r.x < v->b.tb.lines[r.y].l) r.x++;
        return r;
}
static Loc move_cdown(const V *v) {
        Loc r = v->b.loc;
        if (r.y+1 < v->b.tb.l) {
                r.y++;
                r.x = min(r.x, v->b.tb.lines[/*++*/r.y].l); //wg14 y u no ({})
        }
        return r;
}
static Loc move_cup(const V *v) {
        Loc r = v->b.loc;
        if (r.y) {
                r.y--;
                r.x = min(r.x, v->b.tb.lines[r.y].l);
        }
        return r;
}
static Loc move_eol(const V *v) {
	Loc r = v->b.loc;
	r.x = v->b.tb.lines[r.y].l;
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
	while (r.x < tb.lines[r.y].l && !is_space(tb.lines[r.y].glyphs[r.x])) {
		r.x++;
	}
	while (r.x < tb.lines[r.y].l && is_space(tb.lines[r.y].glyphs[r.x])) {
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
        usz l = v->b.tb.lines[v->b.loc.y - 1].l - v->b.loc.x;
        tb_insert(&v->b.tb, v->b.loc.y, 0, v->b.tb.lines[v->b.loc.y - 1].glyphs + v->b.loc.x, l);
        tb_remove(&v->b.tb, v->b.loc.y - 1, v->b.loc.x, l);
        v->b.loc.x = 0;
}
static void undo_ins_nl(V *v, void *state) {
	v->b.loc.y--;
	v->b.loc.x = v->b.tb.lines[v->b.loc.y].l;
	tb_insert(&v->b.tb, v->b.loc.y, v->b.loc.x, v->b.tb.lines[v->b.loc.y + 1].glyphs, v->b.tb.lines[v->b.loc.y + 1].l);
	tb_remove_line(&v->b.tb, v->b.loc.y + 1);
}
Function transform_ins_nl(const V *v) {
	return new_transformation(NULL, perform_ins_nl, undo_ins_nl);
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

static void perform_prep_nl(V *v, void *state) {
	tb_insert_line(&v->b.tb, v->b.loc.y);
	v->b.loc.x = 0;
	v->mode = ModeInsert;
}
static void undo_prep_nl(V *v, void *state) {
	undo_add_nl(v, state);
	v->mode = ModeNormal; //todo wrong
}
Function transform_prep_nl(const V *v) {
	return new_transformation((void*)v->b.loc.x, perform_prep_nl, undo_prep_nl);
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
static void perform_normal(V *v, void *state) { v->mode = ModeNormal; }
Function transform_insert(const V *v) { return new_transformation((void*)(usz)v->mode, perform_insert, undo_modeswitch); }
Function transform_normal(const V *v) { return new_transformation((void*)(usz)v->mode, perform_normal, undo_modeswitch); }

struct if_state { Mode old_mode; usz old_x; };
static void perform_insert_front(V *v, void *state) { v->b.loc.x = 0; v->mode = ModeInsert; }
static void perform_insert_back(V *v, void *state) { v->b.loc.x = v->b.tb.lines[v->b.loc.y].l; v->mode = ModeInsert; }
static void undo_if(V *v, void *state) { struct if_state *s = state; v->b.loc.x = s->old_x; v->mode = s->old_mode; }
Function transform_insert_front(const V *v) { return new_transformation(onew(struct if_state, .old_mode=v->mode, .old_x=v->b.loc.x), perform_insert_front, undo_if); }
Function transform_insert_back(const V *v) { return new_transformation(onew(struct if_state, .old_mode=v->mode, .old_x=v->b.loc.x), perform_insert_back, undo_if); }


static void perform_delete(V *v, void *state) {
	Loc *dst = state;
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

	//intra-line
	if (dst->x < v->b.loc.x) {
		tb_remove(&v->b.tb, dst->y, dst->x, v->b.loc.x - dst->x);
		v->b.loc.x = dst->x;
	} else {
		tb_remove(&v->b.tb, dst->y, v->b.loc.x, dst->x - v->b.loc.x);
	}
}
static void undo_delete(V *v, void *state) { assert(0); /*todo*/ }


static Function deleter(const V *v, void *state, const Function *other) {
	assert (other->type.type == TypeMotion); //wg14 y u no HKT
	Loc *nloc = new(Loc, 1);
	*nloc = other->motion(v);
	return new_transformation(nloc, perform_delete, undo_delete);
}
Function hof_delete(const V *v) {
	return new_function(NULL, TypeTransform, TypeMotion, deleter);
}
