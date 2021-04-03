#ifndef CV_FUNCTIONS_H
#define CV_FUNCTIONS_H

typedef enum {
	TypeBottom,
	TypeStr,          // () -> Str.             e.g. the parameter to '/'.  (And 'i', sort of.)
	TypeChar,         // () -> Char.            e.g. the parameter to 'r' or 't'
	TypeMotion,       // (TB,Loc) -> Loc.       e.g. 'w'
	TypeTransform,    // (TB,Loc) -> (TB,Loc).  e.g. 'x'
	TypeFunction,
	TypeTop,
	// todo text objects?
	// can they replace motions with an implicit 'move' transformation?  (I
	// kinda like that, need to figure out the relationship to input,
	// though; it would never do to have to prefix every movement with an
	// extra command.)
	// see also readme
} TypeType;

typedef struct Type Type;
struct Type {
	TypeType type;
	Type *fn; // (parm,ret)
};

typedef struct Function Function;
struct Function {
	Type type; //-> higher_order if .arity
	//todo defer?
	union {
		struct { const glyph *s; usz l; } str;
		glyph character;
		struct {
			const void *state;
			Loc (*perform)(const V *v, const void *state);
		} motion;
		struct {
			void *state; //todo serialize?
			void (*perform)(V *v, void *state);
			void (*undo)(V *v, void *state);
		} action;
		struct {
			void *state;
			Mode mode;
			Function (*transform)(const V *v, void *state, const Function *other); //todo self to refine bot/top type
		} function;
	};
};

static inline Function new_str(const glyph *s, usz l) { return (Function){.type={TypeStr}, .str.s=s, .str.l=l}; }
static inline Function new_char(glyph g) { return (Function){.type={TypeChar}, .character=g}; }
static inline Function new_motion(const void *state, Loc (*perform)(const V*,const void*)) { return (Function){.type={TypeMotion}, .motion={.state=state, .perform=perform}}; }
static inline Function new_transformation(void *state, void (*perform)(V*,void*), void (*undo)(V*,void*)) { return (Function){.type={TypeTransform}, .action={.state=state, .perform=perform, .undo=undo}}; }
static inline Function new_function(void *state, Mode mode, TypeType ret, TypeType parameter, Function (*transform)(const V*,void*,const Function*)) {
	assert (ret != TypeFunction && parameter != TypeFunction);

	Function f = {0};
	f.type.type = TypeFunction;
	f.type.fn = new(Type, 2);
	f.type.fn[0].type = parameter;
	f.type.fn[1].type = ret;
	f.function.state = state;
	f.function.mode = mode;
	f.function.transform = transform;

	return f;
}

Function motion_cleft(const V*), motion_cright(const V*), motion_cup(const V*), motion_cdown(const V*);

typedef Function (Actor)(const V *b);
Actor motion_cleft, motion_cright, motion_cup, motion_cdown;
Actor motion_bol, motion_eol;
Actor motion_wordforward, motion_wordback;
Actor transform_ins_nl, transform_prep_nl, transform_add_nl, transform_delback, transform_delforward;
Actor transform_insert, transform_normal;
Actor transform_insert_front, transform_insert_back;
Actor hof_delete;

void apply_transformation(V *b, const Function *f);

#endif //CV_FUNCTIONS_H
