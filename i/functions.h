#ifndef CV_FUNCTIONS_H
#define CV_FUNCTIONS_H

typedef enum {
	TypeNil = 0,            // Bottom
	TypeStr = 0x1,          // () -> Str.             e.g. the parameter to '/'.  (And 'i', sort of.)
	TypeChar = 0x2,         // () -> Char.            e.g. the parameter to 'r' or 't'
	TypeMotion = 0x4,       // (TB,Loc) -> Loc.       e.g. 'w'
	TypeTransform = 0x8,    // (TB,Loc) -> (TB,Loc).  e.g. 'x'
	// todo text objects?
	// can they replace motions with an implicit 'move' transformation?  (I
	// kinda like that, need to figure out the relationship to input,
	// though; it would never do to have to prefix every movement with an
	// extra command.)
	// see also readme
} TypeType;

typedef struct Type Type;
struct Type {
	TypeType ret;     //strict
	usz arity;
	Type *param;      //specifies constraints
};

typedef struct Function Function;
struct Function {
	Type type; //-> higher_order if .arity
	//todo defer?
	union {
		struct { const glyph *s; usz l; } str;
		glyph character;
		Loc (*motion)(const V *v);
		struct {
			void *state; //todo serialize?
			void (*perform)(V *v, void *state);
			void (*undo)(V *v, void *state);
		} action;
		struct {
			void *state;
			//needs Function *ret, which is a dummy but still mostly correct.  (ret can be bottom?)  (ret can be bottom if unknowable?)
			Function (*transform)(const V *v, void *state, const Function *other);
		} higher_order;
	};
};

static inline Function new_str(const glyph *s, usz l) { return (Function){.type={TypeStr}, .str.s=s, .str.l=l}; }
static inline Function new_char(glyph g) { return (Function){.type={TypeChar}, .character=g}; }
static inline Function new_motion(Loc (*motion)(const V*)) { return (Function){.type={TypeMotion}, .motion=motion}; }
static inline Function new_transformation(void *state, void (*perform)(V*,void*), void (*undo)(V*,void*)) { return (Function){.type={TypeTransform}, .action={.state=state, .perform=perform, .undo=undo}}; }
static inline Function new_hof(void *state, TypeType ret, TypeType parameter, Function (*transform)(const V*,void*,const Function*)) { return (Function){.type={.ret=ret, .arity=1, .param=memcpy(new(Type,1),&(Type){.ret=parameter},sizeof(Type))}, .higher_order={.state=state, .transform=transform}}; } //todo nice 'cpy'

Function motion_cleft(const V*), motion_cright(const V*), motion_cup(const V*), motion_cdown(const V*);

typedef Function (Actor)(const V *b);
Actor motion_cleft, motion_cright, motion_cup, motion_cdown;
Actor motion_bol, motion_eol;
Actor motion_wordforward, motion_wordback;
Actor transform_ins_nl, transform_add_nl, transform_delback, transform_delforward;
Actor transform_insert, transform_normal;
Actor hof_delete;

void apply_transformation(V *b, const Function *f);

#endif //CV_FUNCTIONS_H
