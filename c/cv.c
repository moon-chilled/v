#define _GNU_SOURCE //strdup, asprintf

#include "v.h"

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

static bool tickit_to_key(const char *s, SpecialKey *k) {
	if (!strcmp(s, "Left")) return *k = SpecialKeyLeft;
	if (!strcmp(s, "Right")) return *k = SpecialKeyRight;
	if (!strcmp(s, "Up")) return *k = SpecialKeyUp;
	if (!strcmp(s, "Down")) return *k = SpecialKeyDown;
	if (!strcmp(s, "Enter")) return *k = SpecialKeyEnter;
	if (!strcmp(s, "Backspace")) return *k = SpecialKeyBackspace;
	if (!strcmp(s, "Delete")) return *k = SpecialKeyDelete;
	if (!strcmp(s, "Escape")) return *k = SpecialKeyEscape;
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

		if (v->mode == ModeInsert) {
			Function f = new_str(new, l);
			apply_transformation(v, &f);
		} else {
			assert(l == 1);
			if (*new < 128 && v->km_normal.ascii[*new]) {
				Function f = v->km_normal.ascii[*new](v);
				apply_transformation(v, &f);
			}
		}
	} else if (info->type == TICKIT_KEYEV_KEY) {
		SpecialKey k;
		if (!tickit_to_key(info->str, &k)) { msg(v, "Unknown key '%s'", info->str); return 1; }
		Keymap *km = v->mode == ModeInsert ? &v->km_insert : &v->km_normal;
		if (km->special[k]) {
			Function f = km->special[k](v);
			apply_transformation(v, &f);
		}
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

void init_v(V *v) {
	v->newest_message = strdup("Welcome to v!");
	tb_insert_line(&v->b.tb, 0);

	v->mode = ModeInsert;

	v->km_insert.special[SpecialKeyLeft] = motion_cleft;
	v->km_insert.special[SpecialKeyRight] = motion_cright;
	v->km_insert.special[SpecialKeyUp] = motion_cup;
	v->km_insert.special[SpecialKeyDown] = motion_cdown;
	v->km_insert.special[SpecialKeyEnter] = transform_ins_nl;
	v->km_insert.special[SpecialKeyBackspace] = transform_delback;
	v->km_insert.special[SpecialKeyDelete] = transform_delforward;
	v->km_insert.special[SpecialKeyEscape] = transform_normal;

	v->km_normal.ascii['x'] = transform_delforward;
	v->km_normal.ascii['i'] = transform_insert;

	v->km_normal.ascii['h'] = motion_cleft;
	v->km_normal.ascii['j'] = motion_cdown;
	v->km_normal.ascii['k'] = motion_cup;
	v->km_normal.ascii['l'] = motion_cright;
	v->km_normal.ascii['0'] = motion_bol;
	v->km_normal.ascii['$'] = motion_eol;
}

int main(void) {
	V v = {0};
	init_v(&v);

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
