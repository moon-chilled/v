#ifndef CV_FUNCTIONS_H
#define CV_FUNCTIONS_H

typedef enum {
	FunctionStr = 0x1,       // () -> Str.             e.g. the parameter to '/'.  (And 'i', sort of.)
	FunctionChar = 0x2,      // () -> Char.            e.g. the parameter to 'r' or 't'
	FunctionMotion = 0x4,    // (TB,Loc) -> Loc.       e.g. 'w'
	FunctionTransform = 0x8, // (TB,Loc) -> (TB,Loc).  e.g. 'x'
	//todo text objects?
	//can they replace motions with an implicit 'move' transformation?  (I
	//kinda like that, need to figure out the relationship to input,
	//though; it would never do to have to prefix every movement with an
	//extra command.)
} FunctionType;

typedef struct {
	FunctionType type;
	//todo param, defer
	union {
		struct { const glyph *s; usz l; } str;
		glyph character;
		Loc (*motion)(const V *v);
		struct {
			void *state; //todo serialize?
			void (*perform)(V *v, void *state);
			void (*undo)(V *v, void *state);
		} action;
	};
} Function;

static inline Function new_str(const glyph *s, usz l) { return (Function){.type=FunctionStr, .str.s=s, .str.l=l}; }
static inline Function new_char(glyph g) { return (Function){.type=FunctionChar, .character=g}; }
static inline Function new_motion(Loc (*motion)(const V*)) { return (Function){.type=FunctionMotion, .motion=motion}; }
static inline Function new_transformation(void *state, void (*perform)(V*,void*), void (*undo)(V*,void*)) { return (Function){.type=FunctionTransform, .action={.state=state, .perform=perform, .undo=undo}}; }

Function motion_cleft(const V*), motion_cright(const V*), motion_cup(const V*), motion_cdown(const V*);

typedef Function (Actor)(const V *b);
Actor motion_cleft, motion_cright, motion_cup, motion_cdown;
Actor motion_bol, motion_eol;
Actor motion_wordforward, motion_wordback;
Actor transform_ins_nl, transform_delback, transform_delforward;
Actor transform_insert, transform_normal;

void apply_transformation(V *b, const Function *f);

#endif //CV_FUNCTIONS_H
