#ifndef config_h__
#define config_h__

#if 1 /*defined(i386 phat linux)*/

#define HAS_C_PACKED_FIELDS
#define BITS_PER_WORD 32
#include <assert.h>

#elif 0 /*defined(h8300)*/

#define BITS_PER_WORD 16
#define assert(x)

#else
#endif

#endif /* config_h__ */
