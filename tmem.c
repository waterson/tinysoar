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

static void
mark_slot_modified(struct agent* agent, struct slot* slot)
{
    struct slot_list* entry;

    /* Has the slot already been marked as modified? */
    for (entry = agent->modified_slots; entry != 0; entry = entry->next) {
        if (compare_slots(&entry->slot, slot))
            return;
    }

    /* Nope. Push it on the list of modified slots for the agent */
    entry = (struct slot_list*) malloc(sizeof(struct slot_list*));

    entry->slot = *slot;
    entry->next = agent->modified_slots;
    agent->modified_slots = entry;
}

void
tmem_init(struct agent* agent)
{
    ht_init(&agent->slots, (ht_key_comparitor_t) compare_slots);
}

struct preference_list*
tmem_get_preferences_for_slot(struct agent* agent, struct slot* slot)
{
    unsigned hash;
    struct ht_entry** entryp;

    hash = hash_slot(slot->id, slot->attr);
    entryp = ht_lookup(&agent->slots, hash, slot);

    return (struct preference_list*)(*entryp ? (*entryp)->value : 0);
}

void
tmem_add_preference(struct agent* agent, struct preference* pref)
{
    struct preference_list* list =
        (struct preference_list*) malloc(sizeof(struct preference_list));

    struct slot key;
    unsigned hash;
    struct ht_entry** entryp;

    key.id   = pref->id;
    key.attr = pref->attr;

    list->preference = pref;
    list->next       = 0;

    hash = hash_slot(pref->id, pref->attr);
    entryp = ht_lookup(&agent->slots, hash, &key);

    if (*entryp) {
        struct ht_entry* entry = *entryp;
        struct preference_list** link =
            (struct preference_list**) &(entry->value);

        list->next = *link;
        *link = list;
    }
    else {
        struct slot* slot = (struct slot*) malloc(sizeof(struct slot));
        slot->id   = pref->id;
        slot->attr = pref->attr;

        ht_add(&agent->slots, entryp, hash, slot, list);
    }

    /* Add to the list of slots that have changed */
    mark_slot_modified(agent, &key);
}


