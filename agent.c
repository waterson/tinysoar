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
 * Routines for maintaining the agent's runtime state.
 */
#include "alloc.h"
#include "soar.h"

#ifdef DEBUG
#include <stdio.h>
#endif

/*
 * For managing the identifier map, which is a contiguous array that
 * is used to recycle identifiers and to associate a `goal level' with
 * each identifier. An identifier's value is used as a (one-based)
 * index into the array.
 *
 * Each entry in the array contains one bit used to determine if the
 * entry is live (i.e., in use in the agent's slots either as by a
 * preference or a wme). The remaining bits are used to assign a `goal
 * level' to the identifer: this is the highest level in the goal
 * hierarchy with which the identifier is associated; i.e., the
 * highest goal in the hierarchy from which the identifier can be
 * reached by following wme links.
 */

/* The number of entries in the agent's initial map. */
#define DEFAULT_ID_MAP_ENTRIES   8

/* The number of bits used for each entry. (XXX we may want to reduce
   to four bits for 16-bit architectures; if so, that'll take a bit
   more re-work than just twiddling constants here.) */
#define ID_MAP_ENTRY_BITS        sizeof(char) * 8
#define ID_MAP_ENTRY_SZ          sizeof(char)

#define ID_ENTRY_USED_MASK       (1 << (ID_MAP_ENTRY_BITS - 1))
#define ID_ENTRY_LEVEL_MASK      ~ID_ENTRY_USED_MASK

#define ASSERT_VALID_LEVEL(l) \
  ASSERT(((l) & ~ID_ENTRY_LEVEL_MASK == 0), ("bad identifier level %d", (l)))

#define ID_ENTRY_IS_USED(e)      ((e) & ID_ENTRY_USED_MASK)
#define MARK_ID_ENTRY_USED(e)    ((e) |= ID_ENTRY_USED_MASK)
#define MARK_ID_ENTRY_FREE(e)    ((e) &= ~ID_ENTRY_USED_MASK)

#define GET_ID_ENTRY_LEVEL(e)    ((e) & ID_ENTRY_LEVEL_MASK)
#define SET_ID_ENTRY_LEVEL(e, l) \
  (ASSERT_VALID_LEVEL(l), (e) &= ~ID_ENTRY_LEVEL_MASK, (e) |= (l))

/*
 * Make an `acceptable, architecture-supported' preference.
 */
#define MAKE_ARCH_PREF(id, attr, value)              \
    wmem_add_preference(agent,                       \
                        (id), (attr), (value),       \
                        preference_type_acceptable,  \
                        support_type_architecture);  \


/*
 * Constants meant for general use. These are used across agent
 * instances.
 */
static bool_t constants_initialized = 0;
symbol_t constants[USER_CONSTANT_BASE];

/*
 * Push a new goal onto the goal stack.
 */
static void
push_goal_id(struct agent *agent, symbol_t goal_id)
{
    struct symbol_list *entry =
        (struct symbol_list *) malloc(sizeof(struct symbol_list));

    struct symbol_list **link = &agent->goals;

    entry->symbol = goal_id;
    entry->next   = 0;

    while (*link)
        link = &((*link)->next);

    *link = entry;
}

/*
 * Pop the deepest goal from the goal stack.
 */
static void
pop_goals(struct agent *agent)
{
    struct symbol_list *goal = agent->goals;

    while (goal) {
        struct symbol_list *doomed = goal;
        goal = goal->next;
        free(doomed);
    }

    agent->goals = 0;
}

/*
 * Initialize the top-state, creating:
 *
 *   (STATE ^superstate nil ^io IO)
 *   (IO ^input-link INPUT ^output-link OUTPUT)
 *
 */
static void
init_top_state(struct agent *agent)
{
    /* The goal stack is completely empty; create initial state */
    symbol_t state, io, input, output;

    agent_reserve_identifiers(agent, 4);
    state  = agent_get_identifier(agent);
    io     = agent_get_identifier(agent);
    input  = agent_get_identifier(agent);
    output = agent_get_identifier(agent);

    push_goal_id(agent, state);

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT),  SYM(NIL_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IO_CONSTANT),          io);
    MAKE_ARCH_PREF(io,    SYM(INPUT_LINK_CONSTANT),  input);
    MAKE_ARCH_PREF(io,    SYM(OUTPUT_LINK_CONSTANT), output);
}

/*
 * Initialize the agent.
 */
void
agent_init(struct agent *agent)
{
    if (! constants_initialized) {
        /* Do one-time initialization */
        int i;

        constants_initialized = 1;

        for (i = NCONSTANTS - 1; i >= 1; --i)
            INIT_SYMBOL(constants[i], symbol_type_symbolic_constant, i);
    }

#ifdef CONF_SOAR_RETE_CREATE
    /* Create a new RETE network if one doesn't exist already. */
    if (! agent->root_node)
        rete_create(agent);
#endif

    rete_init(agent);
    wmem_init(agent);

    agent->goals = 0;
    agent->id_entries = 0;

    agent_reset(agent);
}

/*
 * Finalize the agent.
 */
void
agent_finish(struct agent *agent)
{
    pop_goals(agent);
    wmem_finish(agent);
    rete_finish(agent);

    if (agent->id_entries)
        free(agent->id_entries);
}

/*
 * Reset the agent to its initial state.
 */
void
agent_reset(struct agent *agent)
{
    /* Clear working memory _before_ popping the goal stack so that
       our WME removals can propagate correctly through the RETE
       network. */
    wmem_clear(agent);
    pop_goals(agent);

    /* Reset the identifier map. */
    if (agent->id_entries)
        free(agent->id_entries);

    agent->nid_entries = DEFAULT_ID_MAP_ENTRIES;
    agent->nfree_entries = DEFAULT_ID_MAP_ENTRIES;
    agent->id_entries = malloc(DEFAULT_ID_MAP_ENTRIES * ID_MAP_ENTRY_SZ);
    memset(agent->id_entries, 0, DEFAULT_ID_MAP_ENTRIES * ID_MAP_ENTRY_SZ);

    /* Create a new top state. */
    init_top_state(agent);
}

/*
 * Mark the symbol as `used' in the identifier map if it isn't
 * already. Update the nfree_entries accordingly.
 */
static void
mark_if_unused(struct agent *agent, symbol_t symbol)
{
    if (GET_SYMBOL_TYPE(symbol) == symbol_type_identifier) {
        unsigned char *entry =
            agent->id_entries + (GET_SYMBOL_VALUE(symbol) - 1);

        if (! ID_ENTRY_IS_USED(*entry)) {
            MARK_ID_ENTRY_USED(*entry);
            --agent->nfree_entries;
        }
    }
}

static ht_enumerator_result_t
mark_slot_identifiers(struct ht_entry_header *header, void *closure)
{
    struct slot *slot = (struct slot *) HT_ENTRY_DATA(header);
    struct agent *agent = (struct agent *) closure;
    struct preference *pref;
    struct wme *wme;

    mark_if_unused(agent, slot->id);
    mark_if_unused(agent, slot->attr);

    for (pref = slot->preferences; pref != 0; pref = pref->next_in_slot) {
        mark_if_unused(agent, pref->value);
        if (pref->type & preference_type_binary)
            mark_if_unused(agent, pref->referent);
    }

    for (wme = slot->wmes; wme != 0; wme = wme->next)
        mark_if_unused(agent, wme->value);

    return ht_enumerator_result_ok;
}

/*
 * Try to reclaim unused identifiers.
 */
static void
gc_identifiers(struct agent *agent)
{
    /* Mark all of the identifiers as `free'. */
    char *entry, *limit;
    for (entry = agent->id_entries, limit = agent->id_entries + agent->nid_entries;
         entry < limit;
         ++entry) {
        MARK_ID_ENTRY_FREE(*entry);
    }

    /* Reset the free count to be the size of the entry table. */
    agent->nfree_entries = agent->nid_entries;

    /* Enumerate the slots, marking each used identifier as such, and
       updating nfree_entries appropriately. */
    ht_enumerate(&agent->slots, mark_slot_identifiers, agent);
}

/*
 * Reserve space in the identifier map for at least `count'
 * identifiers.
 */
void
agent_reserve_identifiers(struct agent *agent, int count)
{
    /* If there are enough identifiers to satisfy the request, then
       we're done. */
    if (count <= agent->nfree_entries)
        return;

    /* We don't have enough identifiers to process the request. First,
       let's try to GC old identifiers. */
    gc_identifiers(agent);

    if (count > agent->nfree_entries) {
        /* GC'ing didn't free enough identifiers to satisfy the
           request. Grow the id map. */
        int old_sz = agent->nid_entries * ID_MAP_ENTRY_SZ;
        int new_nid_entries = agent->nid_entries * 2;
        char *new_id_entries = malloc(new_nid_entries * ID_MAP_ENTRY_SZ);

        memcpy(new_id_entries, agent->id_entries, old_sz);
        memset(new_id_entries + old_sz, 0, old_sz);

        agent->nfree_entries += agent->nid_entries;
        agent->nid_entries = new_nid_entries;
        agent->id_entries = new_id_entries;
    }
}

/*
 * Get a new identifier.
 */
symbol_t
agent_get_identifier(struct agent *agent)
{
    unsigned char *entry;
    symbol_t result;

    ASSERT(agent->nfree_entries > 0, ("ran out of identifiers"));
    --agent->nfree_entries;

    /* Scan the map until we find a free entry. */
    entry = agent->id_entries;
    while (ID_ENTRY_IS_USED(*entry))
        ++entry;

    ASSERT(entry < agent->id_entries + agent->nid_entries,
           ("couldn't find a free identifier"));

    /* Mark the entry as in use, and return it. The identifier's value
       is its position in the map, one-indexed. */
    MARK_ID_ENTRY_USED(*entry);
    INIT_SYMBOL(result, symbol_type_identifier, (entry - agent->id_entries) + 1);

    return result;
}

/*
 * Run a single elaboration cycle.
 */
void
agent_elaborate(struct agent *agent)
{
    ASSERT(agent->goals != 0, ("no top-state"));
    wmem_elaborate(agent);
}

/*
 * Handle a state that's reached an operator no-change: pushes a new
 * substate and decorates it appropriately.
 */
void
agent_operator_no_change(struct agent *agent, symbol_t goal)
{
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-no-change => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(NONE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(NO_CHANGE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
}

/*
 * Handle a state that's reached a state no-change: pushes a new
 * substate and decorates it appropriately.
 */
void
agent_state_no_change(struct agent *agent, symbol_t goal)
{
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("state-no-change => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(NONE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(NO_CHANGE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
}

/*
 * Handle a state that's reached an operator conflict: pushes a new
 * substate and decorates it appropriately.
 */
void
agent_operator_conflict(struct agent *agent, symbol_t goal, struct symbol_list *operators)
{
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-conflict => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(MULTIPLE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(CONFLICT_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));

    for ( ; operators != 0; operators = operators->next)
        MAKE_ARCH_PREF(state, SYM(ITEM_CONSTANT), operators->symbol);
}

/*
 * Handle a state that's reached an operator tie: pushes a new
 * substate and decorates it appropriately.
 */
void
agent_operator_tie(struct agent *agent, symbol_t goal, struct symbol_list *operators)
{
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-tie => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(MULTIPLE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(TIE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));

    for ( ; operators != 0; operators = operators->next)
        MAKE_ARCH_PREF(state, SYM(ITEM_CONSTANT), operators->symbol);
}

struct gc_data {
    struct agent *agent;
    struct ht     reachable;
    bool_t        mark_done;
};

static bool_t
compare_symbols(const symbol_t *s1, const symbol_t *s2)
{
    return *s1 == *s2;
}

/*
 * Add the specified symbol to the reachable set.
 */
static void
add_reachable_symbol(struct ht               *reachable,
                     struct ht_entry_header **entryp,
                     symbol_t                 symbol)
{
    struct ht_entry_header *entry =
        (struct ht_entry_header *)
        malloc(sizeof(struct ht_entry_header) + sizeof(symbol_t));

    symbol_t *sym = (symbol_t *) HT_ENTRY_DATA(entry);
    *sym = symbol;

    ht_add(reachable, entryp, symbol, entry);
}

/*
 * Called once for each slot to compute identifiers that are reachable
 * from the current set.
 */
static ht_enumerator_result_t
mark(struct ht_entry_header *header, void *closure)
{
    struct slot *slot = (struct slot *) HT_ENTRY_DATA(header);
    struct gc_data *gc = (struct gc_data *) closure;
    struct ht_entry_header **entryp =
        (struct ht_entry_header **)
        ht_lookup(&gc->reachable, slot->id, &slot->id);

    struct preference *pref;

    if (*entryp) {
        /* This slot's identifier is reachable. Add identifiers
           reachable from this slot to the reachability set. */
        for (pref = slot->preferences; pref != 0; pref = pref->next_in_slot) {
            entryp =
                (struct ht_entry_header **)
                ht_lookup(&gc->reachable, pref->value, &pref->value);

            if (! *entryp) {
                /* Found an identifier that we hadn't reached yet. */
                add_reachable_symbol(&gc->reachable, entryp, pref->value);
                gc->mark_done = 0;
            }
        }
    }

    return ht_enumerator_result_ok;
}

/*
 * Remove preferences for any unreachable identifiers.
 */
static ht_enumerator_result_t
sweep(struct ht_entry_header *header, void *closure)
{
    struct slot *slot = (struct slot *) HT_ENTRY_DATA(header);
    struct gc_data *gc = (struct gc_data *) closure;

    struct ht_entry_header **entryp =
        (struct ht_entry_header **) ht_lookup(&gc->reachable, slot->id, &slot->id);

    if (! *entryp) {
        /* This identifier wasn't reachable. Nuke the preferences and
           WMEs that it contains. */
        struct preference *pref;
        struct wme *wme;

        for (pref = slot->preferences; pref != 0; ) {
            struct preference *doomed = pref;
            pref = pref->next_in_slot;
            wmem_remove_preference(gc->agent, doomed);
        }

        for (wme = slot->wmes; wme != 0; ) {
            struct wme *doomed = wme;
            wme = wme->next;
            rete_operate_wme(gc->agent, doomed, wme_operation_remove);
            free(doomed);
        }

        slot->wmes = 0;
    }

    /* XXX Why can't we remove the slot (i.e., return
       ht_enumerator_result_delete) here? */
    return ht_enumerator_result_ok;
}

static ht_enumerator_result_t
symbol_set_finalizer(struct ht_entry_header *header, void *closure)
{
    return ht_enumerator_result_delete;
}

/*
 * Pop all of the subgoals beneath the specified goal, removing
 * preferences associated with those goals.
 */
void
agent_pop_subgoals(struct agent *agent, struct symbol_list *bottom)
{
    struct gc_data gc;
    struct symbol_list *goal;

    ASSERT(bottom != 0, ("no subgoals to pop"));

    /* Gather all the reachable identifiers: the goal stack is our
       root set. Note that we don't yet truncate the goal stack: we
       need the goal stack to be intact so that we can unwind
       retractions in the RETE network. */
    gc.agent = agent;
    ht_init(&gc.reachable, (ht_key_compare_t) compare_symbols);

    for (goal = agent->goals; goal != bottom->next; goal = goal->next) {
        struct ht_entry_header **entryp =
            (struct ht_entry_header **)
            ht_lookup(&gc.reachable, goal->symbol, &goal->symbol);

        ASSERT(*entryp == 0, ("circular goal list"));

        add_reachable_symbol(&gc.reachable, entryp, goal->symbol);
    }

    /* Compute the transitive closure: iterate until no new
       identifiers are reached. */
    do {
        gc.mark_done = 1;
        ht_enumerate(&agent->slots, mark, &gc);
    } while (! gc.mark_done);

    /* Remove preferences for the non-reachable identifiers. */
    ht_enumerate(&agent->slots, sweep, &gc);

    /* Clean up. */
    ht_finish(&gc.reachable, symbol_set_finalizer, 0);

    /* Nuke goals beneath the new bottom. */
    goal = bottom->next;
    bottom->next = 0;

    do {
        struct symbol_list *doomed = goal;
        goal = goal->next;
        free(doomed);
    } while (goal);
}

