#include "soar.h"
#include "alloc.h"
#include "ht.h"

#ifdef DEBUG
#include <stdio.h>
#endif

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

/*
 * Find the slot with the specified `id' and `attr', creating a new
 * one if necessary when `create' is non-zero.
 */
static struct slot*
find_slot(struct agent* agent, symbol_t id, symbol_t attr, bool_t create)
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
    else if (create) {
        struct ht_entry_header* entry =
            (struct ht_entry_header*) malloc(sizeof(struct ht_entry_header) + sizeof(struct slot));

        slot = (struct slot*) HT_ENTRY_DATA(entry);
        slot->id          = id;
        slot->attr        = attr;
        slot->preferences = 0;
        slot->wmes        = 0;

        ht_add(&agent->slots, entryp, hash, entry);
    }
    else slot = 0;

    return slot;
}

/*
 * Remove the specified slot
 */
static void
remove_slot(struct agent* agent, struct slot* slot)
{
    unsigned hash = hash_slot(slot->id, slot->attr);
    struct ht_entry_header** headerp = ht_lookup(&agent->slots, hash, slot);
    struct ht_entry_header* header = *headerp;

    ASSERT(slot->wmes == 0, ("removing slot while it contains wmes"));
    ASSERT(slot->preferences == 0, ("removing slot while it contains preferences"));

    ASSERT(header != 0, ("attempt to remove non-existent slot"));

    ht_remove(&agent->slots, headerp);
    free(header);
}

/*
 * Mark the specified slot as `modified' by adding to a queue of
 * modified slots.
 */
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

/*
 * Given a list of preferences, compute candidate symbol values.
 */
static void
decide_slot(struct preference* preferences,
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

    return 0;
}

static bool_t
is_context_slot(struct agent* agent, struct slot* slot)
{
    if (SYMBOLS_ARE_EQUAL(SYM(OPERATOR_CONSTANT), slot->attr)) {
        struct symbol_list* goal;
        for (goal = agent->goals; goal != 0; goal = goal->next) {
            if (SYMBOLS_ARE_EQUAL(goal->symbol, slot->id))
                return 1;
        }
    }

    return 0;
}

/*
 * Enumerate the modified slots; for each, ensure that the wmes in the
 * slot correctly reflect the preferences in the slot.
 */
static void
decide_slots(struct agent* agent)
{
    struct slot_list* slots;
    struct slot_list* next;

    for (slots = agent->modified_slots; slots != 0; slots = next) {
        struct preference* pref =
            get_preferences_for_slot(agent, slots->slot);

        bool_t context_slot = is_context_slot(agent, slots->slot);

        next = slots->next;

        if (pref) {
            /* Okay, there are preferences in this slot. Compute the
               candidate values for the slot. */
            struct symbol_list* candidates = 0;
            decide_slot(pref, &candidates);

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
                        /* Make a new wme. Note that wme's that go
                           into a context slot are `acceptable', not
                           `normal'. */
                        wme = (struct wme*) malloc(sizeof(struct wme));
                        wme->slot  = slots->slot;
                        wme->value = candidate->symbol;
                        wme->type  = context_slot ? wme_type_acceptable : wme_type_normal;
                        wme->next  = slots->slot->wmes;
                        slots->slot->wmes = wme;

                        rete_operate_wme(agent, wme, wme_operation_add);
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

                        rete_operate_wme(agent, doomed, wme_operation_remove);
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
            /* There are no preferences for the slot. Nuke the wmes
               and remove the slot */
            struct wme* wme = slots->slot->wmes;
            while (wme) {
                struct wme* doomed = wme;
                wme = wme->next;

                rete_operate_wme(agent, doomed, wme_operation_remove);
                free(doomed);
            }

            slots->slot->wmes = 0;

            remove_slot(agent, slots->slot);
        }

        free(slots);
    }

    agent->modified_slots = 0;
}

/*
 * Add the preference to the appropriate slot
 */
static void
hash_preference(struct agent* agent, struct preference* pref)
{
    struct slot* slot = find_slot(agent, pref->id, pref->attr, 1);

    ASSERT(slot != 0, ("couldn't find a slot"));

    pref->next_in_slot = slot->preferences;
    slot->preferences  = pref;

    /* Add to the list of slots that have changed */
    mark_slot_modified(agent, slot);
}


/*
 * Add a new preference to working memory
 */
void
wmem_add_preference(struct agent* agent,
                    symbol_t id, symbol_t attr, symbol_t value,
                    preference_type_t type,
                    support_type_t support)
{
    struct preference* pref =
        (struct preference*) malloc(sizeof(struct preference));

    pref->next_in_instantiation = 0;
    pref->type    = type;
    pref->support = support;
    pref->id      = id;
    pref->attr    = attr;
    pref->value   = value;

    hash_preference(agent, pref);
}


/*
 * Remove a preference from working memory
 */
void
wmem_remove_preference(struct agent* agent,
                       symbol_t id, symbol_t attr, symbol_t value,
                       preference_type_t type)
{
    struct slot* slot = find_slot(agent, id, attr, 0);

    if (slot) {
        struct preference* doomed;
        struct preference** link;

        for (doomed = slot->preferences, link = &slot->preferences;
             doomed != 0;
             link = &doomed->next_in_slot, doomed = doomed->next_in_slot) {
            if (SYMBOLS_ARE_EQUAL(doomed->value, value) &&
                (doomed->type == type)) {
                *link = doomed->next_in_slot;
                free(doomed); /* XXX safe? */

                /* Add to the list of slots that have changed */
                mark_slot_modified(agent, slot);
                break;
            }
        }
    }
}


/*
 * Closure data for wmem_enumerates_wmes()
 */
struct wme_enumerator_data {
    struct agent*    agent;
    wme_enumerator_t enumerator;
    void*            closure;
};

/*
 * Helper for wmem_enumerate_wmes(): enumerates the wmes in each slot.
 */
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

/*
 * Enumerate all the working memory elements
 */
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

/*
 * Given a right-hand side value (i.e., an `rhs_value'), a token from
 * the RETE network, and a list of unbound variables, compute the
 * symbol that is the `instantiated value' for the right-hand side
 * value.
 */
static symbol_t
instantiate_rhs_value(struct rhs_value* value,
                      struct token* token,
                      struct symbol_list* unbound_vars)
{
    symbol_t result;

    switch (value->type) {
    case rhs_value_type_symbol:
        /* If the rhs_value is a symbol, just return the symbol */
        result = value->val.symbol;
        break;

    case rhs_value_type_variable_binding:
        /* If the rhs_value is a variable binding, then use the token
           to compute the symbol that's bound. */
        result = rete_get_variable_binding(value->val.variable_binding, token);
        break;
        
    case rhs_value_type_unbound_variable:
        /* If the rhs_value is an unbound variable, grovel through the
           instantiated unbound variables to find an identifier. */
        {
            int index = (int) value->val.unbound_variable;
            while (--index > 0)
                unbound_vars = unbound_vars->next;

            result = unbound_vars->symbol;
        }
        break;
    }

    /* Sanity check the result */
    ASSERT(result.type != symbol_type_variable, ("rhs_value bound to variable"));

    return result;
}

/*
 * Instantiate a production.
 */
static void
create_instantiation(struct agent* agent,
                     struct production* production,
                     struct token* token,
                     struct instantiation** instantiations)
{
    struct instantiation* inst =
        (struct instantiation*) malloc(sizeof(struct instantiation));

    struct symbol_list* unbound_vars = 0;
    struct action* action;
    int count;

    /* generate identifiers for the unbound variables */
    for (count = (int) production->num_unbound_vars - 1; count >= 0; --count) {
        struct symbol_list* entry =
            (struct symbol_list*) malloc(sizeof(struct symbol_list));

        entry->symbol = agent_get_identifier(agent);
        entry->next   = unbound_vars;
        unbound_vars  = entry;
    }

    /* initialize the instantiation */
    inst->next        = *instantiations;
    inst->production  = production;
    inst->token       = token;
    inst->preferences = 0;

    *instantiations = inst;

    /* process the right-hand side of the production */
    for (action = production->actions; action != 0; action = action->next) {
        struct preference* pref =
            (struct preference*) malloc(sizeof(struct preference));

        pref->next_in_slot = 0;

        pref->next_in_instantiation = inst->preferences;
        inst->preferences = pref;

        pref->support = 0; /*XXX*/
        pref->type    = action->preference_type;

        pref->id    = instantiate_rhs_value(&action->id, token, unbound_vars);
        pref->attr  = instantiate_rhs_value(&action->attr, token, unbound_vars);
        pref->value = instantiate_rhs_value(&action->value, token, unbound_vars);

        if (action->preference_type & preference_type_binary) {
            ASSERT(SYMBOLS_ARE_EQUAL(pref->attr, SYM(OPERATOR_CONSTANT)),
                   ("binary preference on non-operator"));

            pref->referent = instantiate_rhs_value(&action->referent, token, unbound_vars);
        }
    }

    /* release unbound variables */
    while (unbound_vars) {
        struct symbol_list* doomed = unbound_vars;
        unbound_vars = unbound_vars->next;
        free(doomed);
    }
}

/*
 * Process new matches, adding instantiations for `assertions', and
 * retracting them for `retractions'.
 */
static void
process_matches(struct agent* agent)
{
    struct preference* o_rejects = 0;
    struct instantiation* inst = 0;
    struct match* match;
    struct match* doomed;

#ifdef DEBUG
    printf("Firing:\n");
#endif

    /* create instantiations for assertions */
    match = agent->assertions;
    while (match) {
#ifdef DEBUG
        printf("  %s\n", match->production->name);
#endif
        create_instantiation(agent, match->production, match->data.token, &inst);

        doomed = match;
        match = match->next;
        free(doomed);
    }

    /* The assertions have now been processed */
    agent->assertions = 0;

    /* Process each instantiation */
    while (inst) {
        struct instantiation* next = inst->next;
        struct preference* pref;
        struct preference** link;

        /* Transfer instantiation to the production that `owns' it */
        inst->next = inst->production->instantiations;
        inst->production->instantiations = inst;

        /* Install each preference in working memory (which is what
           Soar8 calls `temporary memory', but we've sorta folded
           together with wmem) */
        for (pref = inst->preferences, link = &inst->preferences;
             pref != 0;
             link = &pref->next_in_instantiation, pref = pref->next_in_instantiation) {
            if ((pref->type == preference_type_reject) &&
                (pref->support == support_type_osupport)) {
                /* Oooh, an o-supported reject preference! These are
                   special, and we'll process them later. Also, we'll
                   splice it out of the list of preferences that this
                   instantiation created, because o-rejects `don't
                   exist'. */
                *link = pref->next_in_instantiation;

                /* Slop! Since we'll not be needing that
                   `next_in_instantiation' field anymore, use it to
                   thread the list of o-supported rejects */
                pref->next_in_instantiation = o_rejects;
                o_rejects = pref;
            }
            else hash_preference(agent, pref);
        }

        /* on to the next one */
        inst = next;
    }

#ifdef DEBUG
    printf("Retracting:\n");
#endif

    match = agent->retractions;
    while (match) {
#ifdef DEBUG
        printf("  %s\n", match->production->name);
#endif

        /* Remove the preferences involved with this instantiation */
        {
            struct preference* pref = match->data.instantiation->preferences;
            struct preference* next;

            while (pref) {
                next = pref->next_in_instantiation;
                wmem_remove_preference(agent, pref->id, pref->attr, pref->value, pref->type);
                pref = next;
            }
        }

        /* Yank the instantiation from the production */
        {
            struct instantiation** link =
                &match->data.instantiation->production->instantiations;

            struct instantiation* inst = *link;

            while (inst) {
                if (inst == match->data.instantiation) {
                    *link = inst->next;
                    free(inst);
                    break;
                }

                link = &inst->next;
                inst = inst->next;
            }
        }

        doomed = match;
        match = match->next;
        free(doomed);
    }

    /* The retractions have now been processed */
    agent->retractions = 0;

    /* Now process o-supported reject preferences. We'll simply clean
       house on the slot, removing *every* preference there is. */
    while (o_rejects) {
        struct preference* rejector = o_rejects;
        struct slot* slot = find_slot(agent, rejector->id, rejector->attr, 0);
        struct preference* rejected = slot->preferences;
        struct preference** link = &slot->preferences;

        while (rejected) {
            struct preference* doomed = rejected;
            *link = rejected->next_in_slot;
            rejected = rejected->next_in_slot;

            if (rejected->support == support_type_architecture) {
                /* You can't remove things the architecture puts in */
                link = &rejected->next_in_slot;
                continue;
            }

#if 0 /* XXX leak! */
            free(doomed); /* XXX ooh, not safe! need to unlink from
                             instantiation, too. */
#endif
        }

        mark_slot_modified(agent, slot);

        o_rejects = o_rejects->next_in_instantiation;
        free(rejector);
    }
}

/*
 * Run an elaboration cycle: decide values for each modified slot, and
 * then process new matches if any exist. Returns `true' if quiescence
 * has been reached (that is, there are no matches to process).
 */
bool_t
wmem_elaborate(struct agent* agent)
{
    decide_slots(agent);

    if (agent->assertions || agent->retractions) {
        process_matches(agent);
        return 0;
    }

    return 1;
}


/*
 * Enumerator callback for wmem_finish()'s first pass: releases all
 * wmes in the slot, and notifies the rete network.
 */
static ht_enumerator_result_t
slot_wme_finalizer(struct ht_entry_header* header, void* closure)
{
    struct agent* agent = (struct agent*) closure;
    struct slot* slot = (struct slot*) HT_ENTRY_DATA(header);

    struct wme* wme = slot->wmes;

    while (wme) {
        struct wme* doomed = wme;
        wme = wme->next;

        /* XXX Hrm, kind of a waste, because we'll pile up a ton
           of retractions. Maybe we should just have a
           `rete_clear()' function? */
        rete_operate_wme(agent, doomed, wme_operation_remove);
        free(wme);
    }

    slot->wmes = 0;

    return ht_enumerator_result_ok;
}

/*
 * Finalizer callback for wmem_finish()'s second pass: releases all
 * preferences in the slot and clobbers it.
 */
static ht_enumerator_result_t
slot_finalizer(struct ht_entry_header* header, void* closure)
{
    struct slot* slot = (struct slot*) HT_ENTRY_DATA(header);
    struct preference* pref = slot->preferences;

    ASSERT(slot->wmes == 0, ("uh oh, somebody created a wme!"));

    while (pref) {
        struct preference* doomed = pref;
        pref = pref->next_in_slot;

        /* Safe at this point, because there should be no
           instantiations to speak of. */
        free(doomed);
    }

    return ht_enumerator_result_delete;
}

/*
 * Finalize working memory, releasing all slots, preferences, and
 * wmes. This will have side effects in the rete network.
 */
void
wmem_finish(struct agent* agent)
{
    /* First, remove *all* wmes. This will clean up the rete network
       properly */
    ht_enumerate(&agent->slots, slot_wme_finalizer, agent);

    /* Unfortunately, as a side effect, it will generate a bazillion
       retractions. Clean them up. (XXX this is expensive, and will
       break if there's a degenerate rule that'd fire in the absence
       of all wmes -- could such an evil rule be made?) */
    process_matches(agent);

    /* Xxx At this point, it'd be nice to assert that there are no
       instantiations. We can't do that now (without grovelling
       through the rete network) because we don't store productions or
       instantiations anywhere else. */

    /* Now, clobber the slots. */
    ht_finish(&agent->slots, slot_finalizer, agent);
}


/*
 * Initialize working memory
 */
void
wmem_init(struct agent* agent)
{
    ht_init(&agent->slots, (ht_key_compare_t) compare_slots);
}


/*
 * Clear working memory.
 */
void
wmem_clear(struct agent* agent)
{
    wmem_finish(agent);
    wmem_init(agent);
}

