#include "wmem.h"

void
wmem_init(struct wmem* wmem)
{
    pool_init(&wmem->wme_pool, sizeof(struct wme), 8);
    wmem->wmes = 0;
}


struct wme*
wmem_add(struct wmem* wmem, symbol_t id, symbol_t attr, symbol_t value, wme_type_t type)
{
    struct wme* wme = (struct wme*) pool_alloc(&wmem->wme_pool);
    wme->id = id;
    wme->attr = attr;
    wme->value = value;
    SET_WME_TYPE(*wme, type);
    SET_WME_NEXT(*wme, wmem->wmes);
    wmem->wmes = wme;
    return wme;
}
