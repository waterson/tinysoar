#ifndef wmem_h__
#define wmem_h__

#include "types.h"

typedef struct wme {
    object_t object;
    symbol_t attribute;
    value_t  value;
} wme_t;

typedef struct working_memory {
    wme_t* wmes;
    int    nwmes;
} working_memory_t;

typedef struct wme_iterator {
    wme_t* current;
} wme_iterator_t;

void
first_wme(working_memory_t* wmem, wme_iterator_t* iterator);

static inline wme_t*
get_wme(wme_iterator_t* iterator)
{
    return iterator->current;
}

void
next_wme(working_memory_t* wmem, wme_iterator_t* iterator);

static inline void
last_wme(working_memory_t* wmem, wme_iterator_t* iterator)
{
    iterator->current = wmem->wmes + wmem->nwmes;
}

static inline int
wme_iterator_equals(wme_iterator_t* left, wme_iterator_t* right)
{
    return left->current == right->current;
}

#endif /* wmem_h__ */
