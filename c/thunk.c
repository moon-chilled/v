#include <sys/mman.h>

#include "v.h"

// s7 refuses to pass me a 'context' pointer or anything like that
// harrumph, I say
// I'll make it myself!

enum {
	THUNKM      = 2/*mov*/ + 8/*addr*/,
	THUNK_LOCAL = 1/*jmp*/ + 4/*disp*/,
	THUNK_DIST  = 2/*mov*/ + 8/*addr*/ + 2/*jmp*/,
};

enum { PAGESZ = 4096 };

#ifdef _WIN32
static u1 arg_offs[4] = {
	1,   //rcx
	2,   //rdx
	010, //r8
	011, //r9
};
#else
static u1 arg_offs[4] = {
	7, //rdi
	6, //rsi
	2, //rdx
	1, //rcx
};
#endif

// todo proper allocator
// w^x makes this a pain
// also, need to come up with a better strategy once we switch to the s7 gc
void *create_thunk(void *function_addr, u1 num_args, ...) {
	static _Thread_local char *mem_start = NULL, *mptr = NULL, *mem_end = NULL;
	va_list ap;
	va_start(ap, num_args);

	usz size_if_local = THUNK_LOCAL + THUNKM * num_args, size_if_distant = THUNK_DIST + THUNKM * num_args;

	char *faddr = function_addr;
	bool small;
	{
		usz sz = mem_end - mptr;
		char *local_end = mptr + size_if_local;
		if (!mptr || sz < ((small=(faddr - local_end == (s4)(faddr - local_end))) ? size_if_local : size_if_distant)) {
			mem_start = mptr = mmap(NULL, PAGESZ, PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0); //leak
			assert (mem_start && mem_start != MAP_FAILED);
			mem_end = mem_start + PAGESZ;

			GC_add_roots(mem_start, mem_end);

			char *local_end = mem_start + size_if_local;
			small = faddr - local_end == (s4)(faddr - local_end);
		} else {
			mprotect(mem_start, PAGESZ, PROT_WRITE); //w NAND x concessions
		}
	}

	char *ret = mptr;

	while (num_args--) {
		int arg_num = va_arg(ap, int);
		assert (arg_num >= 0 && arg_num < sizeof(arg_offs));
		u1 arg = arg_offs[arg_num];
		void *ptr = va_arg(ap, void*);

		*mptr++ = 0x48 + (arg >> 3);        // REX.W.  If bit 4 of the arg is set, then also REX.B
		*mptr++ = 0270 + (arg & 07);        // mov r64, qword
		memcpy(mptr, &ptr, 8); mptr += 8;   // (the qword in question)
	}

	if (small) {
		*mptr++ = 0xe9; // jmp rel32
		memcpy(mptr, &(s4){faddr - (mptr + 4)}, 4); mptr += 4; // (the rel32 in question)
	} else {
		*mptr++ = 0x48; *mptr++ = 0270;     // mov rax, qword
		memcpy(mptr, &faddr, 8); mptr += 8; // (the qword in question)
		*mptr++ = 0xff; *mptr++ = 0340;     // jmp rax
	}

	mprotect(mem_start, PAGESZ, PROT_EXEC);

	va_end(ap);

	return ret;
}
