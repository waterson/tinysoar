#include <malloc.h> /* XXX */
#include "alloc.h"

void*
alloc(unsigned sz)
{
    return malloc(sz);
}
