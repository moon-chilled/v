#include "text-buffer.h"

void tb_insert(TextBuffer *tb, usz ln, usz c, const glyph *text, usz len) {
	assert (ln < tb->l);
	assert (c <= tb->lines[ln].l);
	tb->lines[ln].glyphs = GC_realloc(tb->lines[ln].glyphs, (tb->lines[ln].l + len) * sizeof(glyph));
	memmove(tb->lines[ln].glyphs + c + len, tb->lines[ln].glyphs + c, (tb->lines[ln].l - c) * sizeof(glyph));
	memcpy(tb->lines[ln].glyphs + c, text, len * sizeof(glyph));
	tb->lines[ln].l += len;
}

void tb_remove(TextBuffer *tb, usz ln, usz c, usz extent) {
	assert (ln < tb->l);
	assert (c+extent <= tb->lines[ln].l);
	memmove(tb->lines[ln].glyphs + c, tb->lines[ln].glyphs + c + extent, (tb->lines[ln].l - c - extent) * sizeof(glyph));
	tb->lines[ln].l -= extent;
	tb->lines[ln].glyphs = GC_realloc(tb->lines[ln].glyphs, tb->lines[ln].l * sizeof(glyph));
}

void tb_insert_line(TextBuffer *tb, usz ln) {
	assert (ln <= tb->l);
	tb->lines = GC_realloc(tb->lines, ++tb->l * sizeof(*tb->lines));
	memmove(tb->lines + ln + 1, tb->lines + ln, (tb->l - ln) * sizeof(*tb->lines));
	memset(&tb->lines[ln], 0, sizeof(tb->lines[ln]));
}

void tb_remove_line(TextBuffer *tb, usz ln) {
	assert (ln < tb->l);
	memmove(tb->lines + ln, tb->lines + ln + 1, (tb->l - ln - 1) * sizeof(*tb->lines));
	tb->lines = GC_realloc(tb->lines, --tb->l * sizeof(*tb->lines));
}
