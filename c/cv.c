#define _GNU_SOURCE

#include <tickit.h>

#include "prelude.h"
#include "text-buffer.h"
#include "functions.h"

typedef struct {
	char *newest_message;
	TickitWindow *message_window, *text_window;
	Buffer b;
} V;

usz utf8d1(glyph *o, const unsigned char *i) {
	if (0xf8 <= *i) return 0;
	if (0xf0 <= *i) {
		if (i[1] >> 6 != 2 || i[2] >> 6 != 2 || i[3] >> 6 != 2 ) return 0;
		o[0] = i[0] & 7;
		o[0] <<= 6; o[0] |= i[1] & 0x3f;
		o[0] <<= 6; o[0] |= i[2] & 0x3f;
		o[0] <<= 6; o[0] |= i[3] & 0x3f;
		return 4;
	}

	if (0xe0 <= *i) {
		if (i[1] >> 6 !=2 || i[2] >> 6 != 2) return 0;
		o[0] = i[0] & 15;
		o[0] <<= 6; o[0] |= i[1] & 0x3f;
		o[0] <<= 6; o[0] |= i[2] & 0x3f;
		return 3;
	}

	if (0xc0 <= *i) {
		if (i[1] >> 6 != 2) return 0;
		o[0] = i[0] & 31;
		o[0] <<= 6; o[0] |= i[1] & 0x3f;
		return 2;
	}

	if (0x80 <= *i) return 0;

	*o = *i;
	return 1;
}

const glyph *from_utf8(const unsigned char *text, usz *l) {
	*l = 0;

	glyph *ret = NULL;
	while (*text) {
		ret = GC_realloc(ret, ++*l * sizeof(glyph));
		usz n = utf8d1(ret + *l - 1, text);
		assert(n);
		text += n;
	}

	return ret;
}

typedef enum {
	KeyLeft = 1,
	KeyRight,
	KeyUp,
	KeyDown,
	KeyEnter,
	KeyBackspace,
	KeyDelete,
} Key;

static bool tickit_to_key(const char *s, Key *k) {
	if (!strcmp(s, "Left")) return *k = KeyLeft;
	if (!strcmp(s, "Right")) return *k = KeyRight;
	if (!strcmp(s, "Up")) return *k = KeyUp;
	if (!strcmp(s, "Down")) return *k = KeyDown;
	if (!strcmp(s, "Enter")) return *k = KeyEnter;
	if (!strcmp(s, "Backspace")) return *k = KeyBackspace;
	if (!strcmp(s, "Delete")) return *k = KeyDelete;
	return false;
}

static int msgwin_render(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	V *v = data;
	TickitExposeEventInfo *info = _info;
	tickit_renderbuffer_clear(info->rb);
	tickit_renderbuffer_text_at(info->rb, 0, 0, v->newest_message);
	return 1;
}

static void msg(V *v, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	free(v->newest_message);
	vasprintf(&v->newest_message, fmt, ap);
	tickit_window_expose(v->message_window, NULL);
}

static int on_key(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	TickitKeyEventInfo *info = _info;
	V *v = data;

	if (info->type == TICKIT_KEYEV_TEXT) {
		usz l;
		const glyph *new = from_utf8((const unsigned char*)info->str, &l);

		Function f = new_str(new, l);
		apply_transformation(&v->b, &f);
	} else if (info->type == TICKIT_KEYEV_KEY) {
		Key k;
		if (!tickit_to_key(info->str, &k)) { msg(v, "Unknown key '%s'", info->str); return 1; }
		Function f = {0};
		switch (k) {
			case KeyLeft: f = motion_cleft(&v->b); break;
			case KeyRight: f = motion_cright(&v->b); break;
			case KeyUp: f = motion_cup(&v->b); break;
			case KeyDown: f = motion_cdown(&v->b); break;
			case KeyEnter: f = transform_ins_nl(&v->b); break;
			case KeyBackspace: f = transform_delback(&v->b); break;
			case KeyDelete: f = transform_delforward(&v->b); break;
		}
		if (f.type) apply_transformation(&v->b, &f);
	}

	tickit_window_expose(win, NULL);

	return 1;
}
static int render(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	TickitExposeEventInfo *info = _info;
	V *v = data;
	tickit_renderbuffer_clear(info->rb);

	for (usz i = 0; i < v->b.tb.l; i++) {
		for (usz j = 0; j < v->b.tb.lines[i].l; j++) {
			tickit_renderbuffer_char_at(info->rb, i, j, v->b.tb.lines[i].glyphs[j]);
		}
	}
	tickit_window_set_cursor_position(win, v->b.loc.y, v->b.loc.x);

	return 1;
}

int main(void) {
	V v = {.newest_message=strdup("")};
	tb_insert_line(&v.b.tb, 0);

	Tickit *t = tickit_new_stdio();
	TickitWindow *rt = tickit_get_rootwin(t);
	v.text_window = tickit_window_new(rt, (TickitRect){.top = 0, .left = 0, .lines = tickit_window_lines(rt) - 1, .cols = tickit_window_cols(rt)}, 0);
	v.message_window = tickit_window_new(rt, (TickitRect){.top = tickit_window_lines(rt) - 1, .left = 0, .lines = 1, .cols = tickit_window_cols(rt)}, 0);
	//tickit_term_setctl_int(tickit_window_get_term(rt), TICKIT_TERMCTL_CURSORSHAPE, TICKIT_CURSORSHAPE_LEFT_BAR);
	tickit_window_show(v.text_window);
	tickit_window_show(v.message_window);

	tickit_window_bind_event(v.message_window, TICKIT_WINDOW_ON_EXPOSE, 0, msgwin_render, &v);

	tickit_window_bind_event(v.text_window, TICKIT_WINDOW_ON_KEY, 0, on_key, &v);
	tickit_window_bind_event(v.text_window, TICKIT_WINDOW_ON_EXPOSE, 0, render, &v);
	tickit_window_take_focus(v.text_window);
	tickit_run(t);
	//tickit_term_setctl_int(tickit_window_get_term(rt), TICKIT_TERMCTL_CURSORSHAPE, TICKIT_CURSORSHAPE_BLOCK);
	tickit_window_close(rt);
	tickit_unref(t);
}
