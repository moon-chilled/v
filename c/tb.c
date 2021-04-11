#include "v.h"

//upper 4 bits
//this can be more compact: squish nibbles, upper 3 bits choose byte, then rightshift by (4th bit << 1)
//but meh
//0 are incorrect; they will cause an infinite loop in measure, so easy to diagnose failed validation
static u1 utf8_advance[16] = {
	1, 1, 1, 1, 1, 1, 1, 1, //0...
	0, 0, 0, 0,             //10..!
	2, 2,                   //110.
	3,                      //1110
	4,                      //1111
};

static inline u1 u8_advance(u1 c) { return utf8_advance[c >> 4]; }

bool utf8_validate(const u1 *text, usz bytes) {
	for (const u1 *end = text + bytes; text < end;) {
		u1 d = u8_advance(*text);
		if (!d) return false;
		text += d;
	}
	return true;
}

//todo figure out tabs
static void measure(const u1 *text, usz bytes, usz *graphemes) {
	*graphemes = 0;
	for (const u1 *end = text + bytes; text < end;) {
		u1 adv = u8_advance(*text);
		++*graphemes;
		text += adv;
	}
}
/*
static void count(const u1 *text, usz graphemes, usz *bytes) {
	*bytes = 0;
	for (usz i = 0; i < graphemes; i++) {
		s1 adv = u8_advance(text[*bytes]);
		assert (adv > 0);
		*bytes += adv;
	}
}
*/

void tb_insert(TextBuffer *tb, usz ln, usz byte, const u1 *text, usz bytes, usz *gadv) {
	assert (ln < tb->l);
	assert (byte <= tb->lines[ln].bsz);
	tb->lines[ln].chars = GC_realloc(tb->lines[ln].chars, tb->lines[ln].bsz + bytes);

	memmove(tb->lines[ln].chars + byte + bytes, tb->lines[ln].chars + byte, tb->lines[ln].bsz - byte);
	memcpy(tb->lines[ln].chars + byte, text, bytes);

	measure(text, bytes, gadv);
	tb->lines[ln].bsz += bytes;
	tb->lines[ln].gsz += *gadv;
}

void tb_remove(TextBuffer *tb, usz ln, usz byte, usz bext, usz gext) {
	assert (ln < tb->l);
	assert (byte+bext <= tb->lines[ln].bsz);
	memmove(tb->lines[ln].chars + byte, tb->lines[ln].chars + byte + bext, tb->lines[ln].bsz - byte - bext);
	tb->lines[ln].bsz -= bext;
	tb->lines[ln].gsz -= gext;
	tb->lines[ln].chars = GC_realloc(tb->lines[ln].chars, tb->lines[ln].bsz);
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

struct TextBufferIter {
	TextBuffer *tb;
	Loc cursor;
	TbiMode mode;
	bool forward;
	bool out;
};

//todo invalidate iterators upon mutating
TextBufferIter *tb_iter(TextBuffer *tb, Loc cursor, TbiMode mode, bool forward, bool autosquish) {
	TextBufferIter *ret = onew(TextBufferIter, tb, cursor, mode, forward);
	if (cursor.bx >= tb->lines[cursor.y].bsz && mode == TbiMode_StopBeforeNl) {
		if (!forward && autosquish && tb->lines[cursor.y].bsz) {
			while (!u8_advance(ret->tb->lines[cursor.y].chars[--ret->cursor.bx]));
		} else {
			ret->out = true;
		}
	}
	return ret;
}

void tbi_read(TextBufferIter *tbi, bool advance, const u1 **dst, usz *bsz, usz *vsz) {
	assert(!tbi->out);
	Loc nc = tbi->cursor;
	*vsz = 1; //todo
	if (tbi->forward) {
		if (nc.bx < tbi->tb->lines[nc.y].bsz) {
			*dst = tbi->tb->lines[nc.y].chars + nc.bx;
			*bsz = u8_advance(tbi->tb->lines[nc.y].chars[nc.bx]);
		} else {
			assert (tbi->mode != TbiMode_StopBeforeNl);
			*dst = Cnew(u1, '\n');
			*bsz = 1;

			//should vsz=0?  Not really meaningful...  I guess the
			//'$' has width 1.  But the cursor should never be past
			//that; if the cursor is on it, it's not because its
			//width was actually measured, but because it was
			//brought _past_ all the previous characters.  And the
			//drawing code isn't going to set send_nl anyway...
		}

		if (nc.gx+(tbi->mode == TbiMode_StopBeforeNl) < tbi->tb->lines[nc.y].gsz) {
			nc.bx += u8_advance(tbi->tb->lines[nc.y].chars[nc.bx]);
			nc.gx++;
		} else if (tbi->mode == TbiMode_EatEverything && nc.y+1 < tbi->tb->l) {
			nc.y++;
			nc.gx = nc.bx = 0;
		} else {
			nc.bx += tbi->mode == TbiMode_StopBeforeNl ? u8_advance(tbi->tb->lines[nc.y].chars[nc.bx]) : 1;
			nc.gx++;
			tbi->out = true;
		}
	} else {
		if (nc.bx >= tbi->tb->lines[nc.y].bsz) {
			assert (tbi->mode != TbiMode_StopBeforeNl);
			*dst = Cnew(u1, '\n');
			*bsz = 1;

		} else {
			*dst = tbi->tb->lines[nc.y].chars + nc.bx;
			*bsz = u8_advance(tbi->tb->lines[nc.y].chars[nc.bx]);
		}

		if (nc.bx) {
			nc.gx--;
			while (!u8_advance(tbi->tb->lines[nc.y].chars[--nc.bx]));
		} else {
			if (tbi->mode == TbiMode_EatEverything) {
				nc.y--;
				nc.bx = tbi->tb->lines[nc.y].bsz;
				nc.gx = tbi->tb->lines[nc.y].gsz;
			} else {
				tbi->out = true;
			}
		}
	}

	if (advance) tbi->cursor = nc;
}
Loc tbi_cursor(const TextBufferIter *tbi) { return tbi->cursor; }
bool tbi_out(const TextBufferIter *tbi) { return tbi->out; }



void b_insert(Buffer *b, const u1 *text, usz bytes) {
	assert(utf8_validate(text, bytes));
	usz gadv;
	tb_insert(&b->tb, b->loc.y, b->loc.bx, text, bytes, &gadv);
	b->loc.bx += bytes;
	b->loc.gx += gadv;
}
void b_remove(Buffer *b, Col target) {
	tb_remove(&b->tb, b->loc.y, min(target.bx, b->loc.bx), range(target.bx, b->loc.bx), range(target.gx, b->loc.gx));
	if (target.bx < b->loc.bx) b->loc.col = target;
}
