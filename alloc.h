#ifndef alloc_h__
#define alloc_h__

#include "config.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#else /* ! HAVE_MALLOC_H */

extern void*
malloc(unsigned size);

extern void
free(void* ptr);

#endif /* ! HAVE_MALLOC_H */

#endif /* alloc_h__ */
