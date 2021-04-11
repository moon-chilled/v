#ifndef PRELUDE_H
#define PRELUDE_H

#include <stdint.h>
#include <uchar.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include <gc.h>

typedef size_t usz;
typedef ptrdiff_t ssz;

typedef  uint8_t u1;
typedef   int8_t s1;
typedef uint16_t u2;
typedef  int16_t s2;
typedef uint32_t u4;
typedef  int32_t s4;
typedef uint64_t u8;
typedef  int64_t s8;

#define new(T,n) ((T*)GC_malloc(sizeof(T)*(n)))
#define anew(T,...) ((T*)memcpy(GC_malloc(sizeof((T[]){__VA_ARGS__})),(T[]){__VA_ARGS__},sizeof((T[]){__VA_ARGS__})))
#define onew(T,...) ((T*)memcpy(GC_malloc(sizeof(T)),&(T){__VA_ARGS__},sizeof(T)))
#define cnew(o) (memcpy(GC_malloc(sizeof(o)),&(o),sizeof(o)))
#define Cnew(T, o) (memcpy(GC_malloc(sizeof(T)),&(T){o},sizeof(T)))

#define min(_x, _y) ((_x) < (_y) ? (_x) : (_y))
#define max(_x, _y) ((_x) > (_y) ? (_x) : (_y))
#define range(_x, _y) (max(_x, _y) - min(_x, _y))

#endif //PRELUDE_H
