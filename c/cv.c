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
/* from https://github.com/tmux/tmux/pull/432 */
static int rgb_to_256(u4 clr) {
	u1 r = clr >> 16, g = clr >> 8, b = clr;
	/* Calculate the nearest 0-based colour index at 16 .. 231 */
#define v2ci(v) (v < 48 ? 0 : v < 115 ? 1 : (v - 35) / 40)
	int ir = v2ci(r), ig = v2ci(g), ib = v2ci(b);   /* 0..5 each */
#define colour_index() (36 * ir + 6 * ig + ib)  /* 0..215, lazy evaluation */

	/* Calculate the nearest 0-based gray index at 232 .. 255 */
	int average = (r + g + b) / 3;
	int gray_index = average > 238 ? 23 : (average - 3) / 10;  /* 0..23 */

	/* Calculate the represented colours back from the index */
	static const int i2cv[6] = {0, 0x5f, 0x87, 0xaf, 0xd7, 0xff};
	int cr = i2cv[ir], cg = i2cv[ig], cb = i2cv[ib];  /* r/g/b, 0..255 each */
	int gv = 8 + 10 * gray_index;  // same value for r/g/b, 0..255

	/* Return the one which is nearer to the original input rgb value */
#define dist_square(A,B,C, a,b,c) ((A-a)*(A-a) + (B-b)*(B-b) + (C-c)*(C-c))
	int colour_err = dist_square(cr, cg, cb, r, g, b);
	int gray_err  = dist_square(gv, gv, gv, r, g, b);
	return colour_err <= gray_err ? 16 + colour_index() : 232 + gray_index;
#undef dist_square
#undef colour_index
#undef v2ci
}
static void set_pen_colour(TickitPen *p, u4 clr) {
	tickit_pen_set_colour_attr(p, TICKIT_PEN_FG, rgb_to_256(clr));
	tickit_pen_set_colour_attr_rgb8(p, TICKIT_PEN_FG, (TickitPenRGB8){clr>>16, clr>>8, clr});
}

static int msgwin_render(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	V *v = data;
	TickitExposeEventInfo *info = _info;
	tickit_renderbuffer_clear(info->rb);
	tickit_renderbuffer_text_at(info->rb, 0, 0, v->newest_message);
	return 1;
}
static int mode_render(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	V *v = data;
	TickitExposeEventInfo *info = _info;
	tickit_renderbuffer_clear(info->rb);
	TickitPen *p = tickit_pen_new();
	tickit_pen_set_bool_attr(p, TICKIT_PEN_UNDER, true);
	set_pen_colour(p, 0x4c4c4c);
	tickit_renderbuffer_setpen(info->rb, p);
	tickit_renderbuffer_text_at(info->rb, 0, 0, v->mode == ModeNormal ? "normal" : "insert");
	tickit_pen_unref(p);
	return 1;
}

void msg(V *v, const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(v->newest_message, sizeof(v->newest_message), fmt, ap);
	tickit_window_expose(v->message_window, NULL);
}

Actor *lookupg(V *v, glyph g) {
	if (g >= 128) return NULL;
	if ((v->mode & ModeMotion) && v->km_motion.ascii[g]) return v->km_motion.ascii[g];
	if ((v->mode & ModeTransform) && v->km_transform.ascii[g]) return v->km_transform.ascii[g];
	if ((v->mode & ModeFunction) && v->km_function.ascii[g]) return v->km_function.ascii[g];
	return NULL;
}
Actor *lookup_special(V *v, SpecialKey k) {
	if ((v->mode & ModeMotion) && v->km_motion.special[k]) return v->km_motion.special[k];
	if ((v->mode & ModeTransform) && v->km_transform.special[k]) return v->km_transform.special[k];
	if ((v->mode & ModeFunction) && v->km_function.special[k]) return v->km_function.special[k];

	if (v->mode == ModeInsert) return v->km_insert.special[k];
	return NULL;
}

static int on_key(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	TickitKeyEventInfo *info = _info;
	V *v = data;

	if (info->type == TICKIT_KEYEV_TEXT) {
		usz l;
		const glyph *new = from_utf8((const unsigned char*)info->str, &l);

		if (v->mode == ModeInsert) {
			for (usz i = 0; i < l; i++) {
				Function f = new_char(new[i]);
				v_push(v, &f);
			}
		} else {
			for (usz i = 0; i < l; i++) {
				Actor *a = lookupg(v, new[i]);
				if (a) {
					Function f = a(v);
					v_push(v, &f);
				} else {
					msg(v, "No such command '%c'", new[i]);
				}
			}
		}
	} else if (info->type == TICKIT_KEYEV_KEY) {
		SpecialKey k;
		if (!tickit_to_key(info->str, &k)) { msg(v, "Unknown key '%s'", info->str); return 1; }
		Actor *a = lookup_special(v, k);
		if (a) {
			Function f = a(v);
			v_push(v, &f);
		}
	}

	v_reduce(v);
	tickit_window_expose(win, NULL);

	return 1;
}

static int render(TickitWindow *win, TickitEventFlags flags, void *_info, void *data) {
	TickitExposeEventInfo *info = _info;
	V *v = data;
	tickit_renderbuffer_clear(info->rb);

	TickitPen *normal = tickit_pen_new();
	TickitPen *eol = tickit_pen_new();
	set_pen_colour(eol, 0x0090ee);

	for (usz i = 0; i < v->b.tb.l; i++) {
		tickit_renderbuffer_setpen(info->rb, normal);
		for (usz j = 0; j < v->b.tb.lines[i].l; j++) {
			tickit_renderbuffer_char_at(info->rb, i, j, v->b.tb.lines[i].glyphs[j]);
		}
		tickit_renderbuffer_setpen(info->rb, eol);
		tickit_renderbuffer_char_at(info->rb, i, v->b.tb.lines[i].l, '$');
	}
	tickit_window_set_cursor_position(win, v->b.loc.y, v->b.loc.x);

	tickit_pen_unref(normal);
	tickit_pen_unref(eol);

	tickit_window_expose(v->mode_window, NULL); //todo

	return 1;
}

void init_v(V *v) {
	memset(v, 0, sizeof(*v));

	strcpy(v->newest_message, "Welcome to v!");

	tb_insert_line(&v->b.tb, 0);

	v->mode = ModeNormal;

	v->km_insert.special[SpecialKeyLeft] = motion_cleft;
	v->km_insert.special[SpecialKeyRight] = motion_cright;
	v->km_insert.special[SpecialKeyUp] = motion_cup;
	v->km_insert.special[SpecialKeyDown] = motion_cdown;
	v->km_insert.special[SpecialKeyEnter] = transform_ins_nl;
	v->km_insert.special[SpecialKeyBackspace] = transform_delback;
	v->km_insert.special[SpecialKeyDelete] = transform_delforward;
	v->km_insert.special[SpecialKeyEscape] = transform_normal;

	v->km_motion.ascii['h'] = motion_cleft;
	v->km_motion.ascii['j'] = motion_cdown;
	v->km_motion.ascii['k'] = motion_cup;
	v->km_motion.ascii['l'] = motion_cright;
	v->km_motion.ascii['0'] = motion_bol;
	v->km_motion.ascii['$'] = motion_eol;
	v->km_motion.ascii['w'] = motion_wordforward;
	v->km_motion.ascii['b'] = motion_wordback;
	v->km_transform.ascii['x'] = transform_delforward;
	v->km_transform.ascii['i'] = transform_insert;
	v->km_transform.ascii['o'] = transform_add_nl;
	v->km_transform.ascii['O'] = transform_prep_nl;
	v->km_transform.ascii['I'] = transform_insert_front;
	v->km_transform.ascii['A'] = transform_insert_back;

	v->km_motion.ascii['t'] = hof_move_until;

	v->km_transform.ascii['d'] = hof_delete;
	//todo in normal mode esc should return bottom type (so it gets run immediately) and clear the stack
}

int main(void) {
	V *v = new(V, 1); //big
	init_v(v);

	Tickit *t = tickit_new_stdio();
	TickitWindow *rt = tickit_get_rootwin(t);
	v->text_window = tickit_window_new(rt, (TickitRect){.top = 0, .left = 0, .lines = tickit_window_lines(rt) - 2, .cols = tickit_window_cols(rt)}, 0);
	v->mode_window = tickit_window_new(rt, (TickitRect){.top = tickit_window_lines(rt) - 2, .left = 0, .lines = 1, .cols = tickit_window_cols(rt)}, 0);
	v->message_window = tickit_window_new(rt, (TickitRect){.top = tickit_window_lines(rt) - 1, .left = 0, .lines = 1, .cols = tickit_window_cols(rt)}, 0);
	tickit_window_set_cursor_shape(v->text_window, TICKIT_CURSORSHAPE_LEFT_BAR);
	//tickit_term_setctl_int(tickit_window_get_term(rt), TICKIT_TERMCTL_CURSORSHAPE, TICKIT_CURSORSHAPE_LEFT_BAR);
	tickit_window_show(v->text_window);
	tickit_window_show(v->message_window);

	tickit_window_bind_event(v->mode_window, TICKIT_WINDOW_ON_EXPOSE, 0, mode_render, v);

	tickit_window_bind_event(v->message_window, TICKIT_WINDOW_ON_EXPOSE, 0, msgwin_render, v);

	tickit_window_bind_event(v->text_window, TICKIT_WINDOW_ON_KEY, 0, on_key, v);
	tickit_window_bind_event(v->text_window, TICKIT_WINDOW_ON_EXPOSE, 0, render, v);
	tickit_window_take_focus(v->text_window);
	tickit_run(t);
	tickit_term_setctl_int(tickit_window_get_term(rt), TICKIT_TERMCTL_CURSORSHAPE, TICKIT_CURSORSHAPE_BLOCK);
	tickit_window_close(rt);
	tickit_unref(t);

}
