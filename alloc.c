#include "alloc.h"

#ifndef HAVE_MALLOC_H
void *
malloc(unsigned sz)
{
    return 0;
}

void
free(void *ptr)
{
};
#endif
