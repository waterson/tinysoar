#ifndef pool_h__
#define pool_h__

#include "config.h"

struct pool_block_header;
struct free_list;

struct pool {
    unsigned entry_size;
    unsigned block_size;
    struct pool_block_header* blocks;
    struct free_entry* freelist;
};

void pool_init(struct pool* pool, unsigned entry_size, unsigned block_size);
void* pool_alloc(struct pool* pool);
void pool_free(struct pool* pool, void* ptr);
void pool_finish(struct pool* pool);

#endif /* pool_h__ */

