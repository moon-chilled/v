#ifndef CV_TEXTBUFFER_H
#define CV_TEXTBUFFER_H

typedef struct {
	struct {
		glyph *glyphs; //todo grapheme cluster
		usz l;
	} *lines;
	usz l;
} TextBuffer;

void tb_insert(TextBuffer *buffer, usz ln, usz c, const glyph *text, usz len);
void tb_remove(TextBuffer *buffer, usz ln, usz c, usz extent);
void tb_insert_line(TextBuffer *buffer, usz ln);
void tb_remove_line(TextBuffer *buffer, usz ln);

typedef struct { usz y, x; } Loc; //todo vx

typedef struct {
	TextBuffer tb;
	Loc loc;
} Buffer;

#endif //CV_TEXTBUFFER_H
