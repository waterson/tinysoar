#ifndef config_h__
#define config_h__

#define BEGIN_MACRO do {
#define END_MACRO   } while (0)

typedef unsigned char bool_t;

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

#if defined(__i386) || (_M_IX86 >= 300)
#  define BITS_PER_WORD 32
#elif defined(__H8300__)
#  define BITS_PER_WORD 16
#else
#  error "I can't tell what platform you're on"
#endif

#endif /* config_h__ */
