#ifdef DEBUG

#include <stdarg.h>
#include <string.h>
#include "config.h"

#include <stdio.h>

void
runtime_assert(const char *fmtstr, ...)
{
    va_list ap;

    va_start(ap, fmtstr);
    vfprintf(stderr, fmtstr, ap);
    va_end(ap);

    if (fmtstr[strlen(fmtstr) - 1] != '\n')
        fprintf(stderr, "\n");

#if defined(_MSC_VER) && _M_IX86 >= 300
    __asm { int 3 };
#elif defined(__GNUC__) && defined(__i386)
    asm("int $3");
#endif
}

#endif /* DEBUG */

