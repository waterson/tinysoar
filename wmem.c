#include "wmem.h"

void
working_memory_init(struct working_memory* wmem)
{
    pool_init(&wmem->wme_pool, sizeof(struct wme), 8);
    wmem->wmes = 0;
}
