/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "MPL"); you may not use this file except in
 * compliance with the MPL.  You may obtain a copy of the MPL at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the MPL is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the MPL
 * for the specific language governing rights and limitations under the
 * MPL.
 *
 * The Initial Developer of this code under the MPL is Christopher
 * R. Waterson. Portions created by Christopher R. Waterson are
 * Copyright (C) 2000 Christopher R. Waterson. All Rights Reserved.
 *
 * Contributor(s):
 *   Christopher R. Waterson <waterson@maubi.net>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or 
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 */

/*
 * Working Memory.
 *
 * TODO
 * . wmem_add_preference() needs to take a referent!
 */

#include "soar.h"
#include "alloc.h"
#include "ht.h"

#ifdef DEBUG
#include <stdio.h>
#endif

static bool_t
compare_slots(const struct slot *s1, const struct slot *s2)
{
    return SYMBOLS_ARE_EQUAL(s1->id, s2->id)
        && SYMBOLS_ARE_EQUAL(s1->attr, s2->attr);
}

static inline unsigned
hash_slot(symbol_t id, symbol_t attr)
{
    return ( *((unsigned *)&id)) ^
        ( ( *((unsigned *)&attr) << (BITS_PER_WORD / 2)) |
          ( *((unsigned *)&attr) >> (BITS_PER_WORD / 2)) );
}

/*
 * Find the slot with the specified `id' and `attr', creating a new
 * one if necessary when `create' is non-zero.
 */
static struct slot *
find_slot(struct agent *agent, symbol_t id, symbol_t attr, bool_t create)
{
    unsigned hash = hash_slot(id, attr);
    struct ht_entry_header **entryp;
    struct slot key;
    struct slot *slot;

    key.id   = id;
    key.attr = attr;

    entryp = ht_lookup(&agent->slots, hash, &key);

    if (*entryp)
        slot = (struct slot *) HT_ENTRY_DATA(*entryp);
    else if (create) {
        struct ht_entry_header *entry =
            (struct ht_entry_header *) malloc(sizeof(struct ht_entry_header) + sizeof(struct slot));

        slot = (struct slot *) HT_ENTRY_DATA(entry);
        slot->id          = id;
        slot->attr        = attr;
        slot->preferences = 0;
        slot->wmes        = 0;

        ht_add(&agent->slots, entryp, hash, entry);
    }
    else
        slot = 0;

    return slot;
}

/*
 * Remove the specified slot
 */
static void
remove_slot(struct agent *agent, struct slot *slot)
{
    unsigned hash = hash_slot(slot->id, slot->attr);
    struct ht_entry_header **headerp = ht_lookup(&agent->slots, hash, slot);
    struct ht_entry_header *header = *headerp;

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
mark_slot_modified(struct agent *agent, struct slot *slot)
{
    struct slot_list *entry;

    /* Has the slot already been marked as modified? */
    for (entry = agent->modified_slots; entry != 0; entry = entry->next) {
        if (compare_slots(entry->slot, slot))
            return;
    }

    /* Nope. Push it on the list of modified slots for the agent */
    entry = (struct slot_list *) malloc(sizeof(struct slot_list));

    entry->slot = slot;
    entry->next = agent->modified_slots;
    agent->modified_slots = entry;
}

/*
 * Given a list of preferences, compute candidate symbol values.
 */
static void
collect_candidates(struct preference   *preferences,
                   struct symbol_list **candidates)
{
    struct preference *pref;

    /* Iterate through all the preferences for the slot, adding each
       `acceptable' to the candidate list */
    for (pref = preferences; pref != 0; pref = pref->next_in_slot) {
        if (pref->type == preference_type_acceptable) {
            struct symbol_list *candidate;
            symbol_t value = pref->value;

            for (candidate = *candidates; candidate != 0; candidate = candidate->next) {
                if (SYMBOLS_ARE_EQUAL(candidate->symbol, value))
                    break;
            }

            if (! candidate) {
                candidate = (struct symbol_list *) malloc(sizeof(struct symbol_list));

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
            struct symbol_list *candidate = *candidates;
            struct symbol_list **link = candidates;

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
}

static struct preference *
get_preferences_for_slot(struct agent *agent, struct slot *slot)
{
    unsigned hash = hash_slot(slot->id, slot->attr);
    struct ht_entry_header **entryp
        = ht_lookup(&agent->slots, hash, slot);

    if (*entryp) {
        struct slot *slot = (struct slot *) HT_ENTRY_DATA(*entryp);
        return slot->preferences;
    }

    return 0;
}

struct preference *
wmem_get_preferences(struct agent *agent, symbol_t id, symbol_t attr)
{
    struct slot *slot = find_slot(agent, id, attr, 0);;
    return slot ? slot->preferences : 0;
}

struct wme *
wmem_get_wmes(struct agent *agent, symbol_t id, symbol_t attr)
{
    struct slot *slot = find_slot(agent, id, attr, 0);
    return slot ? slot->wmes : 0;
}

/*
 * Return non-zero if the specified slot's identifier is a goal on the
 * goal stack.
 */
static bool_t
is_operator_slot(struct agent *agent, struct slot *slot)
{
    if (SYMBOLS_ARE_EQUAL(SYM(OPERATOR_CONSTANT), slot->attr)) {
        struct symbol_list *goal;
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
decide_slots(struct agent *agent)
{
    struct slot_list *slots;
    struct slot_list *next;

    for (slots = agent->modified_slots; slots != 0; slots = next) {
        struct preference *pref = slots->slot->preferences;
        bool_t operator_slot = is_operator_slot(agent, slots->slot);

        next = slots->next;

        if (pref) {
            /* Okay, there are preferences in this slot. Compute the
               candidate values for the slot. */
            struct symbol_list *candidates = 0;
            collect_candidates(pref, &candidates);

            /* Add wmes that aren't in the slot */
            {
                struct symbol_list *candidate;
                for (candidate = candidates; candidate != 0; candidate = candidate->next) {
                    struct wme *wme;
                    for (wme = slots->slot->wmes; wme != 0; wme = wme->next) {
                        if (SYMBOLS_ARE_EQUAL(wme->value, candidate->symbol))
                            break;
                    }

                    if (! wme) {
                        /* Make a new wme. Note that wme's that go
                           into a operator slot are `acceptable', not
                           `normal'. */
                        wme = (struct wme *) malloc(sizeof(struct wme));
                        wme->slot  = slots->slot;
                        wme->value = candidate->symbol;
                        wme->type  = operator_slot ? wme_type_acceptable : wme_type_normal;
                        wme->next  = slots->slot->wmes;
                        slots->slot->wmes = wme;

                        rete_operate_wme(agent, wme, wme_operation_add);
                    }
                }
            }

            /* Remove wmes that have no preference */
            {
                struct wme *wme, **link = &slots->slot->wmes;

                while ((wme = *link) != 0) {
                    struct symbol_list *candidate;
                    for (candidate = candidates; candidate != 0; candidate = candidate->next) {
                        if (SYMBOLS_ARE_EQUAL(wme->value, candidate->symbol))
                            break;
                    }

                    if (candidate)
                        link = &wme->next;
                    else {
                        struct wme *doomed = wme;
                        *link = wme->next;
                        rete_operate_wme(agent, doomed, wme_operation_remove);
                        free(doomed);
                    }
                }
            }

            while (candidates) {
                struct symbol_list *doomed = candidates;
                candidates = candidates->next;
                free(doomed);
            }
        }
        else {
            /* There are no preferences for the slot. Nuke the wmes
               and remove the slot */
            struct wme *wme = slots->slot->wmes;
            while (wme) {
                struct wme *doomed = wme;
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
hash_preference(struct agent      *agent,
                symbol_t           id,
                symbol_t           attr,
                struct preference *pref)
{
    struct slot *slot = find_slot(agent, id, attr, 1);

    ASSERT(slot != 0, ("couldn't find a slot"));

    pref->slot         = slot;
    pref->next_in_slot = slot->preferences;
    slot->preferences  = pref;

    /* Add to the list of slots that have changed */
    mark_slot_modified(agent, slot);
}


/*
 * Add a new preference to working memory
 */
struct preference *
wmem_add_preference(struct agent     *agent,
                    symbol_t          id,
                    symbol_t          attr,
                    symbol_t          value,
                    preference_type_t type,
                    support_type_t    support)
{
    struct preference *pref =
        (struct preference *) malloc(sizeof(struct preference));

    /* pref->next_in_slot will be initialized by hash_preference */
    pref->next_in_instantiation =
        pref->prev_in_instantiation =
        pref;

    pref->type    = type;
    pref->support = support;
    pref->value   = value;

    hash_preference(agent, id, attr, pref);
    return pref;
}


/*
 * Remove a preference from working memory
 */
void
wmem_remove_preference(struct agent *agent, struct preference *doomed)
{
    struct slot *slot = doomed->slot;
    struct preference *pref;
    struct preference **link;

    ASSERT(slot != 0, ("no slot"));

    for (link = &slot->preferences; (pref = *link) != 0; link = &pref->next_in_slot) {
        if (pref == doomed) {
            if (pref->prev_in_instantiation) {
                /* Splice the pref out of the instantiation list. */
                pref->prev_in_instantiation->next_in_instantiation
                    = pref->next_in_instantiation;

                pref->next_in_instantiation->prev_in_instantiation
                    = pref->prev_in_instantiation;
            }

            /* Splice the pref out of preferences for the slot */
            *link = pref->next_in_slot;

            free(pref);

            /* Add to the list of slots that have changed */
            mark_slot_modified(agent, slot);
            break;
        }
    }
}


/*
 * Closure data for wmem_enumerates_wmes()
 */
struct wme_enumerator_data {
    struct agent    *agent;
    wme_enumerator_t enumerator;
    void            *closure;
};

/*
 * Helper for wmem_enumerate_wmes(): enumerates the wmes in each slot.
 */
static ht_enumerator_result_t
wme_enumerator_helper(struct ht_entry_header *entry, void *closure)
{
    struct wme_enumerator_data *data =
        (struct wme_enumerator_data *) closure;

    struct slot *slot = (struct slot *) HT_ENTRY_DATA(entry);

    struct wme *wme = slot->wmes;

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
wmem_enumerate_wmes(struct agent     *agent,
                    wme_enumerator_t  enumerator,
                    void             *closure)
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
instantiate_rhs_value(struct rhs_value   *value,
                      struct token       *token,
                      struct symbol_list *unbound_vars)
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
            while (--index >= 0)
                unbound_vars = unbound_vars->next;

            result = unbound_vars->symbol;
        }
        break;

    default:
        UNREACHABLE();
    }

    /* Sanity check the result. */
    ASSERT(GET_SYMBOL_TYPE(result) != symbol_type_variable, ("rhs_value bound to variable"));

    return result;
}

/*
 * Instantiate a production.
 */
static void
create_instantiation(struct agent       *agent,
                     struct production  *production,
                     struct token       *token,
                     struct preference **o_rejects)
{
    struct instantiation *inst =
        (struct instantiation *) malloc(sizeof(struct instantiation));

    struct symbol_list *unbound_vars = 0;
    struct action *action;
    int count;

    /* Initialize the instantiation. */
    inst->production   = production;
    inst->token        = token;
    inst->next         = production->instantiations;

    production->instantiations = inst;

    inst->preferences.next_in_instantiation =
        inst->preferences.prev_in_instantiation = 
        &inst->preferences;

    /* Generate identifiers for the unbound variables. */
    for (count = (int) production->num_unbound_vars - 1; count >= 0; --count) {
        struct symbol_list *entry =
            (struct symbol_list *) malloc(sizeof(struct symbol_list));

        entry->symbol = agent_get_identifier(agent);
        entry->next   = unbound_vars;
        unbound_vars  = entry;
    }

    /* Process the right-hand side of the production. */
    for (action = production->actions; action != 0; action = action->next) {
        struct preference *pref =
            (struct preference *) malloc(sizeof(struct preference));

        symbol_t id, attr;

        pref->next_in_slot = 0;

        pref->type = action->preference_type;

        /* Reconsider preferences must be i-supported, otherwise we'd
           never be able to get rid of them! */
        pref->support = (pref->type == preference_type_reconsider)
            ? support_type_isupport
            : production->support;

        id          = instantiate_rhs_value(&action->id,    token, unbound_vars);
        attr        = instantiate_rhs_value(&action->attr,  token, unbound_vars);
        pref->value = instantiate_rhs_value(&action->value, token, unbound_vars);

        if (action->preference_type & preference_type_binary) {
            WARN_IF(SYMBOLS_ARE_EQUAL(attr, SYM(OPERATOR_CONSTANT)),
                    ("binary preference on non-operator"));

            pref->referent = instantiate_rhs_value(&action->referent, token, unbound_vars);
        }

        if ((pref->type == preference_type_reject) &&
            (pref->support == support_type_osupport)) {
            /* Oooh, an o-supported reject preference! These are
               special, and we'll process them later. We bastardize
               the preferences structure to get the work done: we'll
               fill in the slot `by hand'.

               XXX There's going to be a bit of a problem here if we
               create both acceptable and o-reject preferences for the
               same slot in the same instantiation _if_ the slot
               doesn't exist yet. If the acceptable is created first,
               things will work; if the o-reject is created first, it
               won't mask the acceptable (because find_slot will
               return null). Oh well. */
            pref->slot = find_slot(agent, id, attr, 0);

            if (pref->slot) {
                /* Since we'll not be needing that `next' field
                   anymore, use it to thread the list of o-supported
                   rejects */
                pref->next_in_slot = *o_rejects;
                *o_rejects = pref;
            }
            else {
                /* No slot exists, so just toss the pref and forget we
                   saw the o-reject. */
                free(pref);
            }
        }
        else {
            /* hash the preference into the slots table */
            hash_preference(agent, id, attr, pref);

            /* insert at the tail of the instantiation's list of
               preferences */
            pref->next_in_instantiation = &inst->preferences;
            pref->prev_in_instantiation = inst->preferences.prev_in_instantiation;

            inst->preferences.prev_in_instantiation->next_in_instantiation = pref;
            inst->preferences.prev_in_instantiation = pref;
        }
    }

    /* release unbound variables */
    while (unbound_vars) {
        struct symbol_list *doomed = unbound_vars;
        unbound_vars = unbound_vars->next;
        free(doomed);
    }
}

/*
 * Remove an o-supported preference if it's a duplicate of another
 * o-supported preference.
 */
static void
remove_if_duplicate(struct agent      *agent,
                    struct preference *doomed)
{
    struct preference *pref = doomed->slot->preferences;
    for ( ; pref != 0; pref = pref->next_in_slot) {
        if (pref != doomed &&
            pref->support == support_type_osupport &&
            SYMBOLS_ARE_EQUAL(pref->value, doomed->value)) {
            wmem_remove_preference(agent, doomed);
            return;
        }
    }

    /* If we get here, it's not a duplicate. Splice it out of the
       instantiation to avoid any dangling pointers. */
    doomed->next_in_instantiation = doomed->prev_in_instantiation = 0;
}

/*
 * ``Un-instantiate'' a production.
 */
static void
remove_instantiation(struct agent         *agent,
                     struct instantiation *inst)
{
    /* Yank the instantiation from the production */
    {
        struct instantiation **link =
            &inst->production->instantiations;

        struct instantiation *scan = *link;

        while (scan) {
            if (scan == inst) {
                *link = inst->next;
                break;
            }

            link = &scan->next;
            scan = scan->next;
        }

        ASSERT(scan != 0, ("couldn't find instantiation"));
    }

    /* Remove all the i-supported preferences associated with the
       instantiation. */
    {
        struct preference *pref = inst->preferences.next_in_instantiation;
        while (pref != &inst->preferences) {
            struct preference *next = pref->next_in_instantiation;

            if (pref->support == support_type_isupport) {
                /* If the preference is only i-supported, remove it. */
                wmem_remove_preference(agent, pref);
            }
            else {
                /* If the preference is o-supported, remove it if it's
                   a duplicate of another o-supported preference with
                   the same value. */
                remove_if_duplicate(agent, pref);
            }

            pref = next;
        }
    }

    free(inst);
}

/*
 * Process new matches, adding instantiations for `assertions', and
 * retracting them for `retractions'.
 */
static void
process_matches(struct agent *agent)
{
    struct preference *o_rejects = 0;
    struct match *match;
    struct match *doomed;

#ifdef DEBUG
    printf("Firing:\n");
#endif

    /* create instantiations for assertions */
    match = agent->assertions;
    while (match) {
#ifdef DEBUG
        printf("  %s\n", match->production->name);
#endif
        create_instantiation(agent, match->production, match->data.token, &o_rejects);

        doomed = match;
        match = match->next;
        free(doomed);
    }

    /* The assertions have now been processed */
    agent->assertions = 0;

#ifdef DEBUG
    printf("Retracting:\n");
#endif

    match = agent->retractions;
    while (match) {
#ifdef DEBUG
        printf("  %s\n", match->production->name);
#endif

        /* Remove the preferences involved with this instantiation */
        remove_instantiation(agent, match->data.instantiation);

        doomed = match;
        match = match->next;
        free(doomed);
    }

    /* The retractions have now been processed */
    agent->retractions = 0;

    /* Now process o-supported reject preferences. We'll simply clean
       house on the slot, removing *every* preference there is with
       the same value. */
    while (o_rejects) {
        struct preference *rejector = o_rejects;
        struct slot *slot = find_slot(agent, rejector->slot->id, rejector->slot->attr, 0);

        if (slot) {
            struct preference *pref = slot->preferences;

            while (pref) {
                struct preference *next = pref->next_in_slot;

                /* Nuke the pref if it has the same value, and it's not
                   architecturally supported. */
                if (SYMBOLS_ARE_EQUAL(pref->value, rejector->value) &&
                    (pref->support != support_type_architecture))
                    wmem_remove_preference(agent, pref);

                pref = next;
            }
        }

        mark_slot_modified(agent, slot);

        o_rejects = o_rejects->next_in_slot;
        free(rejector);
    }
}

/*
 * The meat of the operator semantics code. Culls the list of
 * candidate operators, pushing impasses if necessary.
 */
static symbol_t
run_operator_semantics_on(struct agent        *agent,
                          symbol_t             goal,
                          struct preference   *preferences,
                          struct symbol_list **candidates,
                          bool_t               can_make_new_impasse)
{
    struct symbol_list *conflicted = 0;
    struct symbol_list *dominated = 0;
    struct symbol_list *candidate;
    bool_t bests = 0;
    bool_t worsts = 0;
    symbol_t nil;

    CLEAR_SYMBOL(nil);

    /* Collect dominated candidates and detect trivial conflicts. */
    for (candidate = *candidates; candidate != 0; candidate = candidate->next) {
        struct preference *p;
        for (p = preferences; p != 0; p = p->next_in_slot) {
            if ((p->type == preference_type_better ||
                 p->type == preference_type_worse)
                && SYMBOLS_ARE_EQUAL(p->value, candidate->symbol)) {
                struct preference *q;

                /* Note the dominated candidate, with the caveat that
                   a candidate cannot dominate itself. */
                if (! SYMBOLS_ARE_EQUAL(candidate->symbol, p->referent)) {
                    struct symbol_list *entry =
                        (struct symbol_list *) malloc(sizeof(struct symbol_list));

                    if (p->type == preference_type_better) {
                        /* The candidate dominates its referent. */
                        entry->symbol = p->referent;
                    }
                    else {
                        /* The candidate is dominated by its referent. */
                        entry->symbol = candidate->symbol;
                    }

                    entry->next = dominated;
                    dominated = entry;
                }

                /* Iterate through the preferences to see if any of
                   the ``better'' or ``worse'' preferences
                   conflict. */
                for (q = preferences; q != 0; q = q->next_in_slot) {
                    /* A preference can't conflict itself. */
                    if (p == q)
                        continue;

                    if (q->type == preference_type_better ||
                        q->type == preference_type_worse) {
                        /* If |q| is a ``better'' or ``worse''
                           preference, then it may conflict.

                           Specifically, if it's the same type of
                           preference as |p|, but the value and
                           referent are swapped, it will conflict
                           (e.g., 1 > 2 and 2 > 1).

                           Alternatively, if |q|'s type is opposite
                           that of |p|, and both preferences have the
                           same value and referent, it will conflict
                           (e.g., 1 > 2 and 1 < 2).

                           XXX Note that this fails to detect
                           non-trivial conflicts! */
                        if ((SYMBOLS_ARE_EQUAL(p->referent, q->value)
                             && SYMBOLS_ARE_EQUAL(q->referent, p->value)
                             && (p->type == q->type))
                            || (SYMBOLS_ARE_EQUAL(p->value, q->value)
                                && SYMBOLS_ARE_EQUAL(p->referent, q->referent)
                                && (p->type != q->type))) {
                            /* Conflict! Add both |p| and |q| to the
                               conflicted set if they've not been
                               added already. */
                            struct symbol_list *entry;

                            for (entry = conflicted; entry != 0; entry = entry->next) {
                                if (SYMBOLS_ARE_EQUAL(entry->symbol, p->value))
                                    break;
                            }

                            if (! entry) {
                                entry = (struct symbol_list *) malloc(sizeof(struct symbol_list));
                                entry->symbol = p->value;
                                entry->next = conflicted;
                                conflicted = entry;
                            }

                            for (entry = conflicted; entry != 0; entry = entry->next) {
                                if (SYMBOLS_ARE_EQUAL(entry->symbol, q->value))
                                    break;
                            }

                            if (! entry) {
                                entry = (struct symbol_list *) malloc(sizeof(struct symbol_list));
                                entry->symbol = q->value;
                                entry->next = conflicted;
                                conflicted = entry;
                            }
                        }
                    }
                }
            }
            else if (p->type == preference_type_best) {
                /* Remember we've seen a ``best'' preference. */
                bests = 1;
            }
            else if (p->type == preference_type_worst) {
                /* Remember we've seen a ``worst'' preference. */
                worsts = 1;
            }
        }
    }

    /* Remove dominated candidates from the candidate list. If we've
       got conflicts, then doing this is a bit of a waste of time;
       however, it properly cleans up the |dominated| list. */
    while (dominated) {
        struct symbol_list *doomed = dominated;
        struct symbol_list **link = candidates;
        struct symbol_list *c = *link;

        while (c) {
            if (SYMBOLS_ARE_EQUAL(c->symbol, dominated->symbol)) {
                *link = c->next;
                free(c);
            }
            else
                link = &c->next;

            c = *link;
        }

        dominated = dominated->next;
        free(doomed);
    }

    if (conflicted) {
        /* If there are conflicted candidates, then create an
           operator-conflict impasse. */
        if (can_make_new_impasse)
            agent_operator_conflict(agent, goal, conflicted);
        
        while (conflicted) {
            struct symbol_list *doomed = conflicted;
            conflicted = conflicted->next;
            free(doomed);
        }

        return nil;
    }

    /* If we get here, then we've got no conflicts. If we have any
       ``best'' candidates, cull out all others and we'll just choose
       from amongst those.

       Conversely, if we have any ``worst'' candidates, cull them out
       as well. */
    if (bests || worsts) {
        struct symbol_list *worst_candidates = 0;

        struct symbol_list **link;
        for (link = candidates, candidate = *link; candidate != 0; candidate = *link) {
            bool_t best = 0;
            bool_t worst = 0;
            struct preference *p;
            for (p = preferences; p != 0; p = p->next_in_slot) {
                if (SYMBOLS_ARE_EQUAL(p->value, candidate->symbol)) {
                    if (p->type == preference_type_best)
                        best = 1;
                    else if (p->type == preference_type_worst)
                        worst = 1;                        
                }
            }

            if (bests && !best) {
                /* If we have best preferences, and this isn't one of
                   them, get rid of it! */
                *link = candidate->next;
                free(candidate);
            }
            else if (worsts && worst) {
                /* If we have worst preferences, and this is one of
                   them, queue it on to the backup list (in case we've
                   got no non-worst preferences to choose from). */
                *link = candidate->next;
                candidate->next = worst_candidates;
                worst_candidates = candidate;
            }
            else
                link = &candidate->next;
        }

        if (*candidates) {
            /* We've got at least on candidate to choose from. If we'd
               stored any ``worsts'' in the backup list, we can free
               them now. */
            while (worst_candidates) {
                struct symbol_list *doomed = worst_candidates;
                worst_candidates = worst_candidates->next;
                free(doomed);
            }
        }
        else {
            /* No non-worst candidates to choose from! Hopefully we've
               got some ``worsts'' in our backup list... */
            *candidates = worst_candidates;
        }
    }

    /* We'd better have some candidates left! */
    WARN_IF(*candidates != 0, ("culled too many candidates"));

    if (! *candidates) {
        /* Punt and drop into a state no-change. This can occur when
           we have a non-trivial conflict, and this is how Soar8
           handles it. */
        if (can_make_new_impasse)
            agent_state_no_change(agent, goal);

        return nil;
    }

    /* Are the remaining candidates all ``indifferent'' to one another? */
    for (candidate = *candidates; candidate != 0; candidate = candidate->next) {
        struct preference *p;
        for (p = preferences; p != 0; p = p->next_in_slot) {
            if (p->type == preference_type_unary_indifferent)
                break;
        }

        if (! p) {
            /* We couldn't find a unary-indifferent preference for the
               candidate. Is it binary-indifferent to all the other
               remaining candidates? */
            struct symbol_list *referent;
            for (referent = *candidates; referent != 0; referent = referent->next) {
                if (candidate == referent)
                    continue;

                for (p = preferences; p != 0; p = p->next_in_slot) {
                    if (p->type == preference_type_binary_indifferent) {
                        if ((SYMBOLS_ARE_EQUAL(p->value, candidate->symbol)
                             && SYMBOLS_ARE_EQUAL(p->referent, referent->symbol))
                            || (SYMBOLS_ARE_EQUAL(p->value, referent->symbol)
                                && SYMBOLS_ARE_EQUAL(p->referent, candidate->symbol))) {
                            /* Found a binary-indifferent for the
                               candidate and the referent! */
                            break;
                        }
                    }
                }

                /* If we've looked through all the preferences and
                   couldn't find a binary-indifferent for the
                   referent, then there's at least one tie. */
                if (! p)
                    break;
            }

            /* If we broke out of the loop above early, then we found
               a tie. */
            if (referent)
                break;
        }
    }

    if (candidate) {
        /* We found a tie. */
        if (can_make_new_impasse)
            agent_operator_tie(agent, goal, *candidates);

        return nil;
    }

    return (*candidates)->symbol;
}

/*
 * This routine implements the oeprator preference semantics
 */
static symbol_t
run_operator_semantics_for(struct agent *agent,
                           symbol_t      goal,
                           struct slot  *slot,
                           bool_t        can_make_new_impasse)
{
    struct preference *preferences = get_preferences_for_slot(agent, slot);
    struct symbol_list *candidates = 0;
    symbol_t result;

    /* Re-collect the candidate operators.

       XXX We could just collect the acceptable wme's and use those,
       rather than re-do this work... */
    collect_candidates(preferences, &candidates);

    result = run_operator_semantics_on(agent, goal, preferences, &candidates, can_make_new_impasse);

    while (candidates) {
        struct symbol_list *doomed = candidates;
        candidates = candidates->next;
        free(doomed);
    }

    return result;
}

static void
select_operator(struct agent *agent)
{
    unsigned depth = 0;
    struct symbol_list *goal;
    struct symbol_list *bottom;

    ASSERT(agent->goals != 0, ("empty goal stack"));

    for (goal = agent->goals; goal != 0; bottom = goal, goal = goal->next, ++depth) {
        /* Has the previously selected operated been reconsidered? */
        struct slot *slot =
            find_slot(agent, goal->symbol, SYM(OPERATOR_CONSTANT), 0);

        struct wme *wme;
        struct wme **link;
        struct preference *pref;

        /* If there's not even a slot, then there certainly is no
           operator selected for this goal! */
        if (! slot)
            continue;

        /* Look for the operator */
        for (wme = slot->wmes, link = &slot->wmes;
             wme != 0;
             link = &wme->next, wme = wme->next) {
            if (wme->type == wme_type_normal)
                break;
        }

#ifdef DEBUG
        /* Ensure there's only ever one operator selected! */
        if (wme) {
            struct wme *check;
            for (check = wme->next; check != 0; check = check->next) {
                ASSERT(check->type == wme_type_acceptable,
                       ("more than one operator selected"));
            }
        }
#endif

        /* If there's a selected operator, then we need to find a
           reconsider preference for it to avoid an operator
           no-change. */
        if (wme) {
            /* Look for a reconsider preference */
            for (pref = slot->preferences; pref != 0; pref = pref->next_in_slot) {
                if (pref->type == preference_type_reconsider)
                    break;
            }

            if (pref) {
                /* Found a reconsider preference. Remove the selected
                   operator from the operator slot, and notify the
                   rete network. */
                rete_operate_wme(agent, wme, wme_operation_remove);
                *link = wme->next;
                free(wme);

                if (goal->next) {
                    /* We have resolved an operator no-change
                       impasse. */
                    agent_pop_subgoals(agent, goal);
                }
            }
            else if (! goal->next) {
                /* we didn't find a reconsider preference at the
                   bottom-most goal, so we're now at an operator
                   no-change impasse */
                agent_operator_no_change(agent, goal->symbol);
                return;
            }
        }

        /* If we get here, then either no operator was selected, or we
           were able to reconsider the previously selected
           operator. Now we go about figuring out then next
           operator. Let's see if there are any acceptable
           operators for the slot. */
        for (wme = slot->wmes; wme != 0; wme = wme->next) {
            if (wme->type == wme_type_acceptable)
                break;
        }

        if (wme) {
            /* At least one acceptable operator exists. */
            bool_t can_make_new_impasse = (goal->next == 0);
            symbol_t selected_op = wme->value;
            struct wme *wme2;

            /* Is there more than one? */
            for (wme2 = wme->next; wme2 != 0; wme2 = wme2->next) {
                if (wme2->type == wme_type_acceptable)
                    break;
            }

            if (wme2) {
                /* At least one other acceptable operator is
                   present. Run the operator preference semantics on
                   the slot to choose one. If one can't be chosen,
                   create a new impasse if we're in the bottom-most
                   state. */
                selected_op = run_operator_semantics_for(agent, goal->symbol, slot, can_make_new_impasse);
            }

            if (! SYMBOL_IS_NIL(selected_op)) {
                /* We've got a single operator. */
                struct wme *op;

#ifdef DEBUG
                /* XXX this should be done in a callback that the
                   embedding context handles. */
                unsigned i;
                for (i = 0; i < depth; ++i)
                    printf("  ");

                printf("[%d]: [%d]", GET_SYMBOL_VALUE(goal->symbol), GET_SYMBOL_VALUE(selected_op));
                printf("\n");
#endif

                op = (struct wme *) malloc(sizeof(struct wme));
                op->slot  = slot;
                op->value = selected_op;
                op->type  = wme_type_normal;
                op->next  = slot->wmes;
                slot->wmes = op;

                rete_operate_wme(agent, op, wme_operation_add);

                if (goal->next) {
                    /* We just resolved an impasse, so now we need to
                       blow away any substates */
                    agent_pop_subgoals(agent, goal);
                }

                return;
            }

            /* If we get here, then we couldn't find a unique operator
               for this state. If we used to be in the bottom state,
               then we know that a new impasse just got pushed onto
               the goal stack, so bail. */
            if (can_make_new_impasse) {
                ASSERT(goal->next, ("expected a new goal to have been pushed"));
                return;
            }
        }

        /* If we get here, no unique acceptable operator was found in
           this state. Continue on to the next goal. */
    }

    /* If we get here, no acceptable operators were found in _any_
       state, push a new state-no-change goal */
    agent_state_no_change(agent, bottom->symbol);
}


/*
 * Run an elaboration cycle: decide values for each modified slot, and
 * then process new matches if any exist.
 */
void
wmem_elaborate(struct agent *agent)
{
    decide_slots(agent);

    if (!agent->assertions && !agent->retractions) {
        /* We've reached quiescence. Select a new operator */
        select_operator(agent);
    }

    process_matches(agent);
}


/*
 * Enumerator callback for wmem_finish()'s first pass: releases all
 * wmes in the slot, and notifies the rete network.
 */
static ht_enumerator_result_t
slot_wme_finalizer(struct ht_entry_header *header, void *closure)
{
    struct agent *agent = (struct agent *) closure;
    struct slot *slot = (struct slot *) HT_ENTRY_DATA(header);

    struct wme *wme = slot->wmes;

    while (wme) {
        /* XXX Hrm, kind of a waste, because we'll pile up a ton
           of retractions. Maybe we should just have a
           `rete_clear()' function? */
        rete_operate_wme(agent, wme, wme_operation_remove);
        wme = wme->next;
    }

    return ht_enumerator_result_ok;
}

/*
 * Finalizer callback for wmem_finish()'s second pass: releases all
 * preferences in the slot and clobbers it.
 */
static ht_enumerator_result_t
slot_finalizer(struct ht_entry_header *header, void *closure)
{
    struct slot *slot = (struct slot *) HT_ENTRY_DATA(header);
    struct preference *pref = slot->preferences;
    struct wme *wme = slot->wmes;

    while (pref) {
        struct preference *doomed = pref;
        pref = pref->next_in_slot;

        /* Safe at this point, because there should be no
           instantiations to speak of. */
        free(doomed);
    }

    while (wme) {
        struct wme *doomed = wme;
        wme = wme->next;

        free(doomed);
    }

    return ht_enumerator_result_delete;
}

/*
 * Finalize working memory, releasing all slots, preferences, and
 * wmes. This will have side effects in the rete network.
 */
void
wmem_finish(struct agent *agent)
{
    struct slot_list *entry;

    /* First, remove *all* wmes. This will clean up the rete network
       properly */
    ht_enumerate(&agent->slots, slot_wme_finalizer, agent);

    /* Unfortunately, as a side effect, it will generate a bazillion
       retractions. Clean them up. (XXX this is expensive, and will
       break if there's a degenerate rule that'd fire in the absence
       of all wmes -- could such an evil rule be made?) */
    process_matches(agent);

    /* XXX At this point, it'd be nice to assert that there are no
       instantiations. We can't do that now (without grovelling
       through the rete network) because we don't store productions or
       instantiations anywhere else. */

    /* Now, clobber the slots... */
    ht_finish(&agent->slots, slot_finalizer, agent);

    /* ...and clean up the (no longer valid) list of modified slots */
    entry = agent->modified_slots;
    while (entry) {
        struct slot_list *doomed = entry;
        entry = entry->next;
        free(doomed);
    }

    agent->modified_slots = 0;
}


/*
 * Initialize working memory
 */
void
wmem_init(struct agent *agent)
{
    ht_init(&agent->slots, (ht_key_compare_t) compare_slots);
}


/*
 * Clear working memory.
 */
void
wmem_clear(struct agent *agent)
{
    wmem_finish(agent);
    wmem_init(agent);
}

