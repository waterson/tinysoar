#include "wmem.h"

void
first_wme(working_memory_t* wmem, wme_iterator_t* iterator)
{
    wme_t* wme;
    wme_t* last = wmem->wmes + wmem->nwmes;
    for (wme = wmem->wmes; wme < last; ++wme) {
        if (wme->object)
            break;
    }

    iterator->current = wme;
}

void
next_wme(working_memory_t* wmem, wme_iterator_t* iterator)
{
    wme_t* wme = iterator->current;
    wme_t* last = wmem->wmes + wmem->nwmes;
    for ( ; wme < last; ++wme) {
        if (wme->object)
            break;
    }

    iterator->current = wme;
}
