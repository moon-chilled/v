#ifndef CV_V_H
#define CV_V_H

typedef struct V V;

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

typedef enum {
	ModeNormal,
	ModeInsert,
} Mode;

typedef struct {
	Actor *ascii[128];
	Actor *special[SpecialKeyMax];
} Keymap;

struct V {
	char *newest_message;
	TickitWindow *message_window, *text_window;
	Buffer b;
	Mode mode;
	Keymap km_normal, km_insert;
};



#endif //CV_V_H
