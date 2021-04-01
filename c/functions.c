#include "prelude.h"
#include "text-buffer.h"
#include "functions.h"

static Loc move_cleft(const Buffer *b) {
        Loc r = b->loc;
        if (r.x) r.x--;
        return r;
}
static Loc move_cright(const Buffer *b) {
        Loc r = b->loc;
        if (r.x < b->tb.lines[r.y].l) r.x++;
        return r;
}
static Loc move_cdown(const Buffer *b) {
        Loc r = b->loc;
        if (r.y+1 < b->tb.l) {
                r.y++;
                r.x = min(r.x, b->tb.lines[/*++*/r.y].l); //wg14 y u no ({})
        }
        return r;
}
static Loc move_cup(const Buffer *b) {
        Loc r = b->loc;
        if (r.y) {
                r.y--;
                r.x = min(r.x, b->tb.lines[r.y].l);
        }
        return r;
}
Function motion_cleft(const Buffer *b) { return new_motion(move_cleft); }
Function motion_cright(const Buffer *b) { return new_motion(move_cright); }
Function motion_cup(const Buffer *b) { return new_motion(move_cup); }
Function motion_cdown(const Buffer *b) { return new_motion(move_cdown); }

static void perform_ins_nl(Buffer *b, void *state) {
        tb_insert_line(&b->tb, ++b->loc.y);
        b->loc.x = 0;
}
static void undo_ins_nl(Buffer *b, void *state) {
	tb_remove_line(&b->tb, b->loc.y--);
	b->loc.x = (usz)state;
}
Function transform_ins_nl(const Buffer *b) {
	return new_transformation((void*)b->loc.x, perform_ins_nl, undo_ins_nl);
}

static void perform_add_nl(Buffer *b, void *state) {
        tb_insert_line(&b->tb, ++b->loc.y);
        b->loc.x = 0;
}
static void undo_add_nl(Buffer *b, void *state) {
	tb_remove_line(&b->tb, b->loc.y--);
	b->loc.x = (usz)state;
}
Function transform_add_nl(const Buffer *b) {
	return new_transformation((void*)b->loc.x, perform_add_nl, undo_add_nl);
}

static void perform_nothing(Buffer *b, void *state) {}
static void undo_nothing(Buffer *b, void *state) {}

static void perform_delback(Buffer *b, void *state) {
        tb_remove(&b->tb, b->loc.y, --b->loc.x, 1);
}
static void undo_delback(Buffer *b, void *state) {
	tb_insert(&b->tb, b->loc.y, b->loc.x++, &(glyph){(glyph)(usz)state}, 1);
}
Function transform_delback(const Buffer *b) {
	if (!b->loc.x) return new_transformation(NULL, perform_nothing, undo_nothing);
	return new_transformation((void*)(usz)b->tb.lines[b->loc.y].glyphs[b->loc.x-1], perform_delback, undo_delback);
}

static void perform_delforward(Buffer *b, void *state) {
	tb_remove(&b->tb, b->loc.y, b->loc.x, 1);
}
static void undo_delforward(Buffer *b, void *state) {
	tb_insert(&b->tb, b->loc.y, b->loc.x, &(glyph){(glyph)(usz)state}, 1);
}
Function transform_delforward(const Buffer *b) {
	if (b->loc.x >= b->tb.lines[b->loc.y].l) return new_transformation(NULL, perform_nothing, undo_nothing);
	return new_transformation((void*)(usz)b->tb.lines[b->loc.y].glyphs[b->loc.x-1], perform_delforward, undo_delforward);
}
