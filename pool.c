#include <malloc.h> /* XXX */
#include "pool.h"

struct pool_block_header {
    struct pool_block_header* next;
};

struct free_entry {
    struct free_entry* next;
};

void
pool_init(struct pool* pool, unsigned entry_size, unsigned block_size)
{
    /* Must be at least as big as a pointer */
    ASSERT(entry_size >= sizeof(struct free_entry), ("entry size too small"));

    /* Must be alignable to pointer */
    ASSERT(entry_size % sizeof(void*) == 0, ("entry size not alignable"));

    pool->entry_size = entry_size;
    pool->block_size = block_size;
    pool->blocks = 0;
    pool->freelist = 0;
}

void*
pool_alloc(struct pool* pool)
{
    void* result;

    if (! pool->freelist) {
        int i;
        struct free_entry* last_entry = 0;

        /* Allocate a new block */
        struct pool_block_header* block;
        block = (struct pool_block_header*) malloc(sizeof(struct pool_block_header)
                                                   + (pool->entry_size * pool->block_size));
        if (! block)
            return 0;

        for (i = pool->block_size - 1; i >= 0; --i) {
            char* p = ((char*)block) + sizeof(struct pool_block_header) + (i * pool->entry_size);
            struct free_entry* entry = (struct free_entry*) p;
            entry->next = last_entry;
            last_entry = entry;
        }

        block->next = pool->blocks;
        pool->blocks = block;
        pool->freelist = last_entry;
    }

    result = pool->freelist;
    pool->freelist = pool->freelist->next;
    return result;
}

void
pool_free(struct pool* pool, void* ptr)
{
    struct free_entry* entry = ptr;
    entry->next = pool->freelist;
    pool->freelist = entry;
}


void
pool_finish(struct pool* pool)
{
    while (pool->blocks) {
        struct pool_block_header* doomed = pool->blocks;
        pool->blocks = pool->blocks->next;
        free(doomed);
    }

    pool->blocks = 0;
    pool->freelist = 0;
}


