#include "soar.h"
#include "alloc.h"

void
wmem_init(struct agent* agent)
{
    agent->wmes = 0;
}


struct wme*
wmem_add(struct agent* agent, symbol_t id, symbol_t attr, symbol_t value, wme_type_t type)
{
    struct wme* wme = (struct wme*) malloc(sizeof(struct wme));
    wme->id = id;
    wme->attr = attr;
    wme->value = value;
    SET_WME_TYPE(*wme, type);
    SET_WME_NEXT(*wme, agent->wmes);
    agent->wmes = wme;
    return wme;
}


void
wmem_decide(struct agent* agent)
{
    struct slot_list* slots;
    for (slots = agent->modified_slots; slots != 0; slots = slots->next) {
        struct preference_list* preferences =
            tmem_get_preferences_for_slot(agent, &slots->slot);

        if (! preferences) {
            /* possibly remove the slot altogether */
        }
    }
}
