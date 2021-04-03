#ifndef CV_V_H
#define CV_V_H

typedef struct V V;

typedef enum {
	ModeDefault = -1,
	ModeInsert = 0,
	ModeMotion = 0x1,
	ModeTransform = 0x2,
	ModeFunction = 0x4,

	ModeNormal = ModeMotion | ModeTransform | ModeFunction, //todo this needs an ordering
} Mode;

#include <tickit.h>
#include "prelude.h"
#include "text-buffer.h"
#include "functions.h"

typedef enum {
	SpecialKeyLeft = 1,
	SpecialKeyRight,
	SpecialKeyUp,
	SpecialKeyDown,
	SpecialKeyEnter,
	SpecialKeyBackspace,
	SpecialKeyDelete,
	SpecialKeyEscape,
	SpecialKeyMax,
} SpecialKey;

typedef struct {
	Function *ascii[128];
	Function *special[SpecialKeyMax];
} Keymap;

struct V {
	char newest_message[4096];
	TickitWindow *message_window, *mode_window, *text_window;
	Buffer b;
	Mode mode;

	Keymap km_motion, km_transform, km_function;
	Keymap km_insert; //only for specials

	struct {
		Function f;
		Mode old_mode;
	} *stack;
	usz sp;
};

void msg(V *v, const char *fmt, ...);

void v_push(V *v, Function *f);
bool v_reduce(V *v);

#endif //CV_V_H
