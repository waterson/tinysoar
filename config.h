#ifndef config_h__
#define config_h__

#if defined(__i386) || defined(_WIN32)

#define BITS_PER_WORD 32
#include <assert.h>

#elif defined(__H8300__)

#define BITS_PER_WORD 16
#define assert(x)

#else
#error "I can't tell what platform you're on"
#endif

typedef unsigned char bool_t;

#define BEGIN_MACRO do {
#define END_MACRO   } while (0)

#endif /* config_h__ */
