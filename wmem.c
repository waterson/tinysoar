#include "soar.h"
#include "alloc.h"

#include "ht.h"

static bool_t
compare_slots(const struct slot* s1, const struct slot* s2)
{
    return SYMBOLS_ARE_EQUAL(s1->id, s2->id)
        && SYMBOLS_ARE_EQUAL(s1->attr, s2->attr);
}

static inline unsigned
hash_slot(symbol_t id, symbol_t attr)
{
    return ( *((unsigned*)&id)) ^
        ( ( *((unsigned*)&attr) << (BITS_PER_WORD / 2)) |
          ( *((unsigned*)&attr) >> (BITS_PER_WORD / 2)) );
}

static struct slot*
ensure_slot(struct agent* agent, symbol_t id, symbol_t attr)
{
    unsigned hash = hash_slot(id, attr);
    struct ht_entry_header** entryp;
    struct slot key;
    struct slot* slot;

    key.id   = id;
    key.attr = attr;

    entryp = ht_lookup(&agent->slots, hash, &key);

    if (*entryp) {
        slot = (struct slot*) HT_ENTRY_DATA(*entryp);
    }
    else {
        struct ht_entry_header* entry =
            (struct ht_entry_header*) malloc(sizeof(struct ht_entry_header) + sizeof(struct slot));

        slot = (struct slot*) HT_ENTRY_DATA(entry);
        slot->id          = id;
        slot->attr        = attr;
        slot->preferences = 0;
        slot->wmes        = 0;

        ht_add(&agent->slots, entryp, hash, entry);
    }

    return slot;
}

static void
mark_slot_modified(struct agent* agent, struct slot* slot)
{
    struct slot_list* entry;

    /* Has the slot already been marked as modified? */
    for (entry = agent->modified_slots; entry != 0; entry = entry->next) {
        if (compare_slots(entry->slot, slot))
            return;
    }

    /* Nope. Push it on the list of modified slots for the agent */
    entry = (struct slot_list*) malloc(sizeof(struct slot_list*));

    entry->slot = slot;
    entry->next = agent->modified_slots;
    agent->modified_slots = entry;
}

static void
decide_slot(struct agent* agent,
            struct slot* slot,
            struct preference* preferences,
            struct symbol_list** candidates)
{
    struct preference* pref;

    /* Iterate through all the preferences for the slot, adding each
       `acceptable' to the candidate list */
    for (pref = preferences; pref != 0; pref = pref->next_in_slot) {
        if (pref->type == preference_type_acceptable) {
            struct symbol_list* candidate;
            symbol_t value = pref->value;

            for (candidate = *candidates; candidate != 0; candidate = candidate->next) {
                if (SYMBOLS_ARE_EQUAL(candidate->symbol, value))
                    break;
            }

            if (! candidate) {
                candidate = (struct symbol_list*) malloc(sizeof(struct symbol_list));

                candidate->symbol = pref->value;
                candidate->next = *candidates;

                *candidates = candidate;
            }
        }
    }

    /* Iterate through all the preferences again, removing any
       candidates that are masked by `prohibit' or `reject'
       preferences */
    for (pref = preferences; pref != 0; pref = pref->next_in_slot) {
        if (pref->type == preference_type_prohibit ||
            pref->type == preference_type_reject) {
            struct symbol_list* candidate = *candidates;
            struct symbol_list** link = candidates;

            while (candidate != 0) {
                if (SYMBOLS_ARE_EQUAL(candidate->symbol, pref->value)) {
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
    ht_init(&agent->slots, (ht_key_compare_t) compare_slots);
}


struct wme*
wmem_add(struct agent* agent,
         symbol_t id,
         symbol_t attr,
         symbol_t value,
         wme_type_t type)
{
    struct slot* slot = ensure_slot(agent, id, attr);
    struct wme* wme;

    wme = (struct wme*) malloc(sizeof(struct wme));
    wme->slot  = slot;
    wme->value = value;
    wme->type  = type;
    wme->next  = slot->wmes;
    slot->wmes = wme;
    return wme;
}


static struct preference*
get_preferences_for_slot(struct agent* agent, struct slot* slot)
{
    unsigned hash = hash_slot(slot->id, slot->attr);
    struct ht_entry_header** entryp
        = ht_lookup(&agent->slots, hash, slot);

    if (*entryp) {
        struct slot* slot = (struct slot*) HT_ENTRY_DATA(*entryp);
        return slot->preferences;
    }
    else {
        return 0;
    }
}

void
wmem_decide(struct agent* agent)
{
    struct slot_list* slots;
    for (slots = agent->modified_slots; slots != 0; slots = slots->next) {
        struct preference* pref =
            get_preferences_for_slot(agent, slots->slot);

        if (pref) {
            struct symbol_list* candidates = 0;
            decide_slot(agent, slots->slot, pref, &candidates);

            /* Add wmes that aren't in the slot */
            {
                struct symbol_list* candidate;
                for (candidate = candidates; candidate != 0; candidate = candidate->next) {
                    struct wme* wme;
                    for (wme = slots->slot->wmes; wme != 0; wme = wme->next) {
                        if (SYMBOLS_ARE_EQUAL(wme->value, candidate->symbol))
                            break;
                    }

                    if (! wme) {
                        wme = (struct wme*) malloc(sizeof(struct wme));
                        wme->slot  = slots->slot;
                        wme->value = candidate->symbol;
                        wme->type  = wme_type_normal;
                        wme->next  = slots->slot->wmes;
                        slots->slot->wmes = wme;

                        rete_add_wme(agent, wme); /*XXX*/
                    }
                }
            }

            /* Remove wmes that have no preference */
            {
                struct wme* wme = slots->slot->wmes;
                struct wme** link = &slots->slot->wmes;

                while (wme) {
                    struct symbol_list* candidate;
                    for (candidate = candidates; candidate != 0; candidate = candidate->next) {
                        if (SYMBOLS_ARE_EQUAL(wme->value, candidate->symbol))
                            break;
                    }

                    if (candidate) {
                        link = &wme->next;
                        wme = wme->next;
                    }
                    else {
                        struct wme* doomed = wme;
                        *link = wme->next;
                        wme = wme->next;

                        rete_remove_wme(agent, doomed); /*XXX*/
                        free(doomed);
                    }
                }
            }

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


void
wmem_add_preference(struct agent* agent, struct preference* pref)
{
    struct slot* slot = ensure_slot(agent, pref->id, pref->attr);

    pref->next_in_slot = slot->preferences;
    slot->preferences  = pref;

    /* Add to the list of slots that have changed */
    mark_slot_modified(agent, slot);
}


struct wme_enumerator_data {
    struct agent*    agent;
    wme_enumerator_t enumerator;
    void*            closure;
};

static ht_enumerator_result_t
wme_enumerator_helper(struct ht_entry_header* entry, void* closure)
{
    struct wme_enumerator_data* data =
        (struct wme_enumerator_data*) closure;

    struct slot* slot = (struct slot*) HT_ENTRY_DATA(entry);

    struct wme* wme = slot->wmes;

    while (wme) {
        (*data->enumerator)(data->agent, wme, data->closure);
        wme = wme->next;
    }

    return ht_enumerator_result_ok;
}

void
wmem_enumerate_wmes(struct agent* agent,
                    wme_enumerator_t enumerator,
                    void* closure)
{
    struct wme_enumerator_data data;
    data.agent      = agent;
    data.enumerator = enumerator;
    data.closure    = closure;
    ht_enumerate(&agent->slots, wme_enumerator_helper, &data);
}
