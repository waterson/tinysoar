#ifndef config_h__
#define config_h__

#include "config-defs.h"

/* How many bits per word are there? */
#if SIZEOF_INT == 8
#  define BITS_PER_WORD 64
#elif SIZEOF_INT == 4
#  define BITS_PER_WORD 32
#elif SIZEOF_INT == 2
#  define BITS_PER_WORD 16
#endif

/* Wrappers for macros */
#define BEGIN_MACRO do {
#define END_MACRO   } while (0)

/* A type for booleans */
typedef unsigned char bool_t;

/* More verbose equivalents of `assert()' */
#ifdef DEBUG
   extern void
   runtime_assert(const char* fmtstr, ...);

#  define ASSERT(cond, args)   \
     BEGIN_MACRO               \
       if (! (cond))           \
         runtime_assert args ; \
     END_MACRO

#  define ERROR(args)     ASSERT(0, args)
#  define UNIMPLEMENTED() ASSERT(0, ("unimplemented"))
#  define UNREACHABLE()   ASSERT(0, ("unreachable"))

#else
#  define ASSERT(cond, args)
#  define ERROR(args)
#  define UNIMPLEMENTED()
#  define UNREACHABLE()
#endif

#endif /* config_h__ */
