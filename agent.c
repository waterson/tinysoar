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
 * Constants meant for general use. These are used across agent
 * instances.
 */
static bool_t constants_initialized = 0;
symbol_t constants[USER_CONSTANT_BASE];

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
#define ID_ENTRY_MAX_LEVEL       ((unsigned char) ID_ENTRY_LEVEL_MASK)

#define ASSERT_VALID_LEVEL(l) \
  ASSERT(((l) & ~ID_ENTRY_LEVEL_MASK) == 0, ("bad identifier level %d", (l)))

#define ID_ENTRY_IS_USED(e)      ((e) & ID_ENTRY_USED_MASK)
#define MARK_ID_ENTRY_USED(e)    ((e) |= ID_ENTRY_USED_MASK)
#define MARK_ID_ENTRY_FREE(e)    ((e) &= ~ID_ENTRY_USED_MASK)

#define GET_ID_ENTRY_LEVEL(e)    ((e) & ID_ENTRY_LEVEL_MASK)
#define SET_ID_ENTRY_LEVEL(e, l) \
  (ASSERT_VALID_LEVEL(l), (e) &= ~ID_ENTRY_LEVEL_MASK, (e) |= (l))
#define INIT_ID_ENTRY(e, u, l)   \
  (ASSERT_VALID_LEVEL(l), ((e) = ((u) ? ID_ENTRY_USED_MASK : 0) | (l)))

/*
 * Make an `acceptable, architecture-supported' preference.
 */
#define MAKE_ARCH_PREF(id, attr, value)              \
    wmem_add_preference(agent,                       \
                        (id), (attr), (value),       \
                        preference_type_acceptable,  \
                        support_type_architecture);  \


/*
 * Make a preference for an `^item' augmentation on the impasse. This
 * creates a dummy instantiation, complete with token, so that its
 * possible to backtrace to the original proposal.
 */
static void
make_item_pref(struct agent      *agent,
               symbol_t           superstate,
               struct goal_stack *goal,
               symbol_t           operator)
{
#ifdef CONF_SOAR_CHUNKING
    struct instantiation *inst;
    struct token *token;
    struct wme *wme;

    struct preference *pref =
#endif
        MAKE_ARCH_PREF(goal->symbol, SYM(ITEM_CONSTANT), operator);

#ifdef CONF_SOAR_CHUNKING
    /* Find the wme that corresponds to the operator's acceptable
       preference. */
    for (wme = wmem_get_wmes(agent, superstate, SYM(OPERATOR_CONSTANT));
         wme != 0;
         wme = wme->next) {
        if ((wme->state == wme_state_live) &&
            (wme->type == wme_type_acceptable) &&
            (wme->value == operator)) {
            break;
        }
    }

    ASSERT(wme != 0,
           ("unable to find wme for operator [%d] in superstate [%d]",
            GET_SYMBOL_VALUE(operator), GET_SYMBOL_VALUE(superstate)));

    /* Create a dummy token through which we'll reach the wme. */
    token = (struct token *) malloc(sizeof(struct token));
    token->node   = 0;
    token->next   = 0;
    token->parent = 0;
    token->wme    = wme;

    /* Create a dummy instantiation to reach the token. */
    inst = (struct instantiation *) malloc(sizeof(struct instantiation));
    inst->production  = 0;
    inst->token       = token;
    inst->preferences = 0;
    inst->next        = goal->instantiations;

    /* Tie the preference to the instantiation. */
    pref->instantiation = inst;

    /* Add to the set of `dummy' instantiations maintained by the
       goal. We'll need to clean them up by hand when the subgoal gets
       popped off the stack.*/
    inst->next = goal->instantiations;
    goal->instantiations = inst;
#endif
}

/*
 * Push a new goal onto the goal stack.
 */
static struct goal_stack *
push_goal_id(struct agent *agent, symbol_t id)
{
    struct goal_stack **link, *entry;
    int level;

    for (link = &agent->goals, level = 1; *link != 0; ++level)
        link = &((*link)->next);

    entry = (struct goal_stack *) malloc(sizeof(struct goal_stack));
    entry->symbol         = id;
    entry->instantiations = 0;
    entry->next           = 0;

    *link = entry;

    agent_set_id_level(agent, id, level);

    return entry;
}

/*
 * Pop all of the goals from the goal stack.
 */
static void
pop_goals(struct agent *agent)
{
    struct goal_stack *goals = agent->goals;

    if (goals) {
        if (goals->next)
            agent_pop_subgoals(agent, goals);

        ASSERT(goals->instantiations == 0,
               ("instantiations not allowed in top goal"));

        free(goals);
        agent->goals = 0;
    }
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

/*
 * Mark each identifier that's touched by this slot as `used'.
 */
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
    INIT_ID_ENTRY(*entry, 1, 0);
    INIT_SYMBOL(result, symbol_type_identifier, (entry - agent->id_entries) + 1);

    return result;
}

/*
 * Get the level of an identifier.
 */
int
agent_get_id_level(struct agent *agent, symbol_t id)
{
    unsigned char *entry;

    ASSERT(GET_SYMBOL_TYPE(id) == symbol_type_identifier,
           ("attempt to set id level on non-identifier"));

    entry = agent->id_entries + (GET_SYMBOL_VALUE(id) - 1);
    ASSERT(entry < agent->id_entries + agent->nid_entries,
           ("invalid identifier"));

    return GET_ID_ENTRY_LEVEL(*entry);
}

/*
 * Set the level of an identifier.
 */
void
agent_set_id_level(struct agent *agent, symbol_t id, int level)
{
    unsigned char *entry;

    ASSERT(GET_SYMBOL_TYPE(id) == symbol_type_identifier,
           ("attempt to set id level on non-identifier"));

    ASSERT(level > 0 && level < ID_ENTRY_MAX_LEVEL,
           ("invalid level %d", level));

    entry = agent->id_entries + (GET_SYMBOL_VALUE(id) - 1);
    ASSERT(entry < agent->id_entries + agent->nid_entries,
           ("invalid identifier"));

    SET_ID_ENTRY_LEVEL(*entry, level);
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
agent_operator_no_change(struct agent *agent, symbol_t superstate)
{
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-no-change => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), superstate);
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
agent_state_no_change(struct agent *agent, symbol_t superstate)
{
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("state-no-change => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), superstate);
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
agent_operator_conflict(struct agent       *agent,
                        symbol_t            superstate,
                        struct symbol_list *operators)
{
    struct goal_stack *goal;
    symbol_t state;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    goal = push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-conflict => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), superstate);
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(MULTIPLE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(CONFLICT_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));

    for ( ; operators != 0; operators = operators->next)
        make_item_pref(agent, superstate, goal, operators->symbol);
}

/*
 * Handle a state that's reached an operator tie: pushes a new
 * substate and decorates it appropriately.
 */
void
agent_operator_tie(struct agent       *agent,
                   symbol_t            superstate,
                   struct symbol_list *operators)
{
    symbol_t state;
    struct goal_stack *goal;

    agent_reserve_identifiers(agent, 1);
    state = agent_get_identifier(agent);

    goal = push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-tie => [%d]\n", GET_SYMBOL_VALUE(state));
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), superstate);
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(MULTIPLE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(TIE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));

    for ( ; operators != 0; operators = operators->next)
        make_item_pref(agent, superstate, goal, operators->symbol);
}

struct gc_data {
    struct agent *agent;
    int           level;
};

/*
 * Remove preferences for any unreachable identifiers.
 */
static ht_enumerator_result_t
sweep(struct ht_entry_header *header, void *closure)
{
    struct slot *slot = (struct slot *) HT_ENTRY_DATA(header);
    struct gc_data *gc = (struct gc_data *) closure;

    if (agent_get_id_level(gc->agent, slot->id) > gc->level) {
        struct preference *pref;
        struct wme *wme;

        for (pref = slot->preferences; pref != 0; ) {
            struct preference *doomed = pref;
            pref = pref->next_in_slot;
            wmem_remove_preference(gc->agent, doomed, 0);
        }

        for (wme = slot->wmes; wme != 0; ) {
            struct wme *doomed = wme;

            if (wme->state == wme_state_live)
                rete_operate_wme(gc->agent, doomed, wme_operation_remove);

            wme = wme->next;
            free(doomed);
        }

        slot->wmes = 0;
    }
#ifdef DEBUG
    else {
        /* Sanity check to make sure that nothing that's still alive
           refers to an identifier from a level that's being gc'd.

           XXX could we make this stronger and assert that nothing
           refers to a level below the identifier's level? I.e., to
           ensure that all promotion has been done properly? */
        struct preference *pref;
        struct wme *wme;

        for (pref = slot->preferences; pref != 0; pref = pref->next_in_slot) {
            if (GET_SYMBOL_TYPE(pref->value) == symbol_type_identifier) {
                ASSERT(agent_get_id_level(gc->agent, pref->value) <= gc->level,
                       ("preference refers to a value from a gc'd level"));
            }

            if ((pref->type & preference_type_binary) &&
                GET_SYMBOL_TYPE(pref->referent) == symbol_type_identifier) {
                ASSERT(agent_get_id_level(gc->agent, pref->referent) <= gc->level,
                       ("preference refers to a referent from a gc'd level"));
            }
        }

        for (wme = slot->wmes; wme != 0; wme = wme->next) {
            if (GET_SYMBOL_TYPE(wme->value) == symbol_type_identifier) {
                ASSERT(agent_get_id_level(gc->agent, wme->value) <= gc->level,
                       ("wme refers to a value from a gc'd level"));
            }
        }
    }
#endif

    /* XXX Why can't we remove the slot (i.e., return
       ht_enumerator_result_delete) here? */
    return ht_enumerator_result_ok;
}

/*
 * Pop all of the subgoals beneath the specified goal, removing
 * preferences associated with those goals.
 */
void
agent_pop_subgoals(struct agent *agent, struct goal_stack *bottom)
{
    struct goal_stack *goal;
    struct gc_data gc = { agent, 1 };

    ASSERT(bottom != 0, ("no subgoals to pop"));

    /* Determine the level we're popping. */
    for (goal = agent->goals; goal != bottom; goal = goal->next)
        ++gc.level;

    /* Remove preferences for the non-reachable identifiers. */
    ht_enumerate(&agent->slots, sweep, &gc);

    /* Nuke goals beneath the new bottom. */
    goal = bottom->next;
    bottom->next = 0;

    do {
        struct goal_stack *doomed = goal;
#ifdef CONF_SOAR_CHUNKING
        struct instantiation *inst;

        inst = doomed->instantiations;
        while (inst) {
            struct instantiation *next = inst->next;
            wmem_remove_instantiation(agent, inst, 1);
            inst = next;
        }
#endif

        goal = goal->next;
        free(doomed);
    } while (goal);
}

/*
 * Return non-zero if the specified identifier is a goal on the goal
 * stack.
 */
bool_t
agent_is_goal(struct agent *agent, symbol_t id)
{
    struct goal_stack *goal;
    for (goal = agent->goals; goal != 0; goal = goal->next) {
        if (SYMBOLS_ARE_EQUAL(goal->symbol, id))
            return 1;
    }

    return 0;
}
