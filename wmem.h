#ifndef wmem_h__
#define wmem_h__

#include "types.h"
#include "pool.h"

struct wme {
    symbol_t id;
    symbol_t attr;
    symbol_t value;
    struct wme* next;
};

struct working_memory {
    struct wme* wmes;
    struct pool wme_pool;
};

extern void
working_memory_init(struct working_memory* wmem);

#endif /* wmem_h__ */
