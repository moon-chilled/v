#ifndef CV_TEXTBUFFER_H
#define CV_TEXTBUFFER_H

/* TODO btree (~rope)
+compacting; something like this:

compact thread does forever:

acqure mutex
if (sigsetjmp(jmp_buf)) {
	release mutex
	return
}
compact(&buffer)
release mutex

handle(SIGUSR1) {
	if (mutex held) siglongjmp(jmp_buf)
}


main thread does forever:

get command
if (!try acquire mutex) {
	signal(compact thread, SIGUSR1)
	acquire mutex
}
process command
release mutex


This can be smoother.  In particular, the compact function can mutate
(reentrantly) and have smaller work units.  This increases the amount of work
it has to do if it doesn't get interrupted, but reduces the amount of work it
has to throw away if it does; I think it's overall worthwhile.
*/

/* more thoughts on compacting:

 - two outsize options:
   1. try to compact the entire rope into a single string
   2. binary compaction: always merge two leaves into a bigger leaf
 - #2 throws away little work, but has to do a lot more overall work
 - neither is very good
 - I think the algorithmically optimal solution is to always try to halve the
   depth of the tree
 - (omega: I think this is O(n lgn), compared with O(n) and O(n²))

 - ^^ is probably a good backup solution--for dealing with pathological cases--
   but a better algorithm for 99% of real situations, based on gap buffers:
 1. Reserve an überleaf, with enough space for the whole file.  (For posix,
    overcommit will take care of it; for windows, should use VA separate
    reserve/commit.)
 2. Start merging leaves from the beginning until the cursor position.
 3. If you get that far: reserve another überleaf after the cursor position,
    and merge until the end of the file
 4. Following an interruption: if the first leaf was not an uberleaf, goto 1.
    Otherwise, check if the mutation changed said leaf.  If so, free the
    remainder of the leaf and go to 3.  Otherwise continue doing 2 2.  (Todo in
    the former case, should the memory used by the 'remainder' portion be
    reused for the second half of the gap buffer?
*/

typedef struct { usz gx, bx; } Col;
typedef struct {
	usz y;

	// if you create an ABI where this aliasing is not valid, you are horrible and I hate you
	union {
		struct { usz gx, bx; };
		Col col;
	};
} Loc;

typedef struct {
	struct {
		u1 *chars;
		usz bsz, gsz;
		// byte size, grapheme size
		// (currently just using codepoints as a proxy for graphemes)
		// need some representation of visual offset/columns, but that's difficult because of context-dependent things like tabs, so prefer to defer that to draw time
	} *lines;
	usz l;
	u8 generation;
} TextBuffer;
typedef struct TextBufferIter TextBufferIter;

typedef enum {
	TbiMode_StopBeforeNl,
	TbiMode_StopAfterNl,
	TbiMode_EatEverything,
} TbiMode;

// THESE FUNCTIONS DO NOT VALIDATE
// if you pass in invalid utf8, they will die and you will be sad
void tb_insert(TextBuffer *buffer, usz ln, usz byte, const u1 *text, usz bytes, usz *gadv); //gadv is returned
void tb_remove(TextBuffer *buffer, usz ln, usz byte, usz bext, usz gext); // gext is trusted.  YOU BETTER GET IT RIGHT FUCKER
void tb_insert_line(TextBuffer *buffer, usz ln);
void tb_remove_line(TextBuffer *buffer, usz ln);
Loc tb_cursor_at(const TextBuffer *buffer, usz ln, usz grapheme);
TextBufferIter *tb_iter(TextBuffer *buffer, Loc cursor, TbiMode mode, bool forward, bool autosquish);
TextBufferIter *tbi_clone(TextBufferIter *tbi);
void tbi_read(TextBufferIter *tbi, bool advance, const u1 **dst, usz *bsz, usz *vsz); //vsz is visual size e.g. double-width, tab...
bool tbi_out(const TextBufferIter *tbi);
Loc tbi_cursor(const TextBufferIter *tbi);

typedef struct {
	TextBuffer tb;
	Loc loc;
} Buffer;

void b_insert(Buffer *b, const u1 *text, usz bytes);
void b_remove(Buffer *b, Col dst);

#endif //CV_TEXTBUFFER_H
