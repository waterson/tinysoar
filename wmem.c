#include "soar.h"
#include "pool.h"

void
wmem_init(struct agent* agent)
{
    pool_init(&agent->wme_pool, sizeof(struct wme), 8);
    agent->wmes = 0;
}


struct wme*
wmem_add(struct agent* agent, symbol_t id, symbol_t attr, symbol_t value, wme_type_t type)
{
    struct wme* wme = (struct wme*) pool_alloc(&agent->wme_pool);
    wme->id = id;
    wme->attr = attr;
    wme->value = value;
    SET_WME_TYPE(*wme, type);
    SET_WME_NEXT(*wme, agent->wmes);
    agent->wmes = wme;
    return wme;
}
