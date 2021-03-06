#ifndef CV_V_H
#define CV_V_H

typedef struct V V;

typedef enum {
	ModeDefault = -1,
	ModeInsert = 0,
	ModeMotion = 0x1,
	ModeMutate = 0x2,
	ModeFunction = 0x4,

	ModeNormal = ModeMotion | ModeMutate | ModeFunction, //todo this needs an ordering
} Mode;

typedef enum {
	HookVInit,
	HookMax,
} Hook;

#include <tickit.h>
#include "s7.h"
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
	SpecialKeyTab,
	SpecialKeyMax,
} SpecialKey;

typedef struct {
	Function *ascii[128];
	Function *special[SpecialKeyMax];
} Keymap;

typedef struct VV VV;

typedef struct {
	void *env;
	s7_pointer init_state;
	struct hi_ret { u4 colour; s7_pointer state; } (*highlight)(VV *vv, void *env, s7_pointer old_state, TextBufferIter *tbi);
} Highlighter;

struct V {
	VV *vv;
	s7_pointer env;
	char newest_message[4096];
	TickitWindow *message_window, *mode_window, *text_window;
	Buffer b;
	Mode mode;
	AppliedFunction most_recent;
	Highlighter highlighter;

	struct {
		Function f;
		Mode old_mode;
	} *stack;
	usz sp;
};
struct VV {
	V *v; //todo multiple
	s7_scheme *s;
	s7_pointer sym_v, sym_text_buffer_iter;
	s7_pointer sym_default, sym_insert, sym_normal, sym_bottom, sym_str, sym_motion, sym_mutation, sym_function, sym_top;
	s7_pointer sym_stop_before_nl, sym_stop_after_nl, sym_eat_everything;
	s7_pointer sym_y, sym_gx;
	s7_pointer sym_left, sym_right, sym_up, sym_down, sym_enter, sym_backspace, sym_delete, sym_escape, sym_tab;
	s7_pointer sym_procedure_p, sym_character_p, sym_c_pointer_p, sym_c_object_p, sym_symbol_p, sym_not, sym_pair_p, sym_list_p, sym_integer_p, sym_string_p, sym_boolean_p;

	s7_int tag_loc;
	s7_int tag_function_motion, tag_function_function, tag_function_mutation;

	Keymap km_motion, km_mutate, km_function;
	Keymap km_insert; //only for specials

	//(*hooks[mode])(v)
	//void (*(*hooks[HookMax]))(V *v)
};

void vs7_init(VV *vv);
void vs7_deinit(VV *vv);
void msg(V *v, const char *fmt, ...);

void v_push(V *v, Function *f);
bool v_reduce(V *v);

void *create_thunk(void *function_addr, u1 num_args, ...);

#endif //CV_V_H
