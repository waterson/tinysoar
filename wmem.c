#include "soar.h"
#include "alloc.h"

static void
decide_slot(struct agent* agent,
            struct slot* slot,
            struct preference_list* preferences,
            struct symbol_list** candidates)
{
    struct preference_list* entry;

    /* Iterate through all the preferences for the slot, adding each
       `acceptable' to the candidate list */
    for (entry = preferences; entry != 0; entry = entry->next) {
        if (entry->preference->type == preference_type_acceptable) {
            struct symbol_list* candidate;
            symbol_t value = entry->preference->value;

            for (candidate = *candidates; candidate != 0; candidate = candidate->next) {
                if (SYMBOLS_ARE_EQUAL(candidate->symbol, value))
                    break;
            }

            if (! candidate) {
                candidate = (struct symbol_list*) malloc(sizeof(struct symbol_list));

                candidate->symbol = entry->preference->value;
                candidate->next = *candidates;

                *candidates = candidate;
            }
        }
    }

    /* Iterate through all the preferences again, removing any
       candidates that are masked by `prohibit' or `reject'
       preferences */
    for (entry = preferences; entry != 0; entry = entry->next) {
        if (entry->preference->type == preference_type_prohibit ||
            entry->preference->type == preference_type_reject) {
            struct symbol_list* candidate = *candidates;
            struct symbol_list** link = candidates;

            while (candidate != 0) {
                if (SYMBOLS_ARE_EQUAL(candidate->symbol, entry->preference->value)) {
                    *link = candidate->next;
                    free(candidate);
                    break;
                }

                link = &candidate->next;
                candidate = candidate->next;
            }
        }
    }

    /* If we were trying to decide the value for anything but an
       ^operator slot (?), then we're done. */
}

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

        if (preferences) {
            struct symbol_list* candidates = 0;
            decide_slot(agent, &slots->slot, preferences, &candidates);

            /* XXX do wme foo here! */

            while (candidates) {
                struct symbol_list* doomed = candidates;
                candidates = candidates->next;
                free(doomed);
            }
        }
        else {
            /* possibly remove the slot altogether */
        }
    }
}




