#include <stdarg.h>
#include <stdio.h>
#include "config.h"

#ifdef DEBUG
void
runtime_assert(const char* fmtstr, ...)
{
    va_list ap;

    va_start(ap, fmtstr);
    vfprintf(stderr, fmtstr, ap);
    va_end(ap);

#if defined(_MSC_VER) && _M_IX86 >= 300
    __asm { int 3 };
#elif defined(__GNUC__) && defined(__i386)
    asm("int $3");
#endif
}
#endif

