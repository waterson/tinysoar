#ifndef alloc_h__
#define alloc_h__

#include "config.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#else /* ! HAVE_MALLOC_H */

extern void
heap_init(char *addrs[], int naddrs);

extern void *
malloc(unsigned size);

extern void
free(void *ptr);

#if defined(DEBUG) && defined(HAVE_PRINTF)
extern void
heap_walk();
#endif

#endif /* ! HAVE_MALLOC_H */

#endif /* alloc_h__ */
