#include "alloc.h"
#include "soar.h"

#ifdef DEBUG
#include <stdio.h>
#endif

/*
 * Make an `acceptable, architecture-supported' preference.
 */
#define MAKE_ARCH_PREF(id, attr, value)              \
    wmem_add_preference(agent,                       \
                        (id), (attr), (value),       \
                        preference_type_acceptable,  \
                        support_type_architecture);  \


/*
 * Constants meant for general use.
 */
static bool_t constants_initialized = 0;
symbol_t constants[USER_CONSTANT_BASE];

static void
push_goal_id(struct agent* agent, symbol_t goal_id)
{
    struct symbol_list* entry =
        (struct symbol_list*) malloc(sizeof(struct symbol_list));

    struct symbol_list** link = &agent->goals;

    entry->symbol = goal_id;
    entry->next   = 0;

    while (*link)
        link = &((*link)->next);

    *link = entry;
}

static symbol_t
pop_goal_id(struct agent* agent)
{
    struct symbol_list* doomed = agent->goals;
    struct symbol_list** link = &agent->goals;
    symbol_t last;

    ASSERT(doomed != 0, ("popped too many goals"));

    while (doomed->next) {
        link = &doomed->next;
        doomed = doomed->next;
    }

    last = doomed->symbol;
    free(doomed);

    *link = 0;

    return last;

}

/*
 * Initialize the top-state, creating:
 *
 *   (STATE ^superstate nil ^io IO)
 *   (IO ^input-link INPUT ^output-link OUTPUT)
 *
 */
static void
init_top_state(struct agent* agent)
{
    /* The goal stack is completely empty; create initial state */
    symbol_t state  = agent_get_identifier(agent);
    symbol_t io     = agent_get_identifier(agent);
    symbol_t input  = agent_get_identifier(agent);
    symbol_t output = agent_get_identifier(agent);

    push_goal_id(agent, state);

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT),  SYM(NIL_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IO_CONSTANT),          io);
    MAKE_ARCH_PREF(io,    SYM(INPUT_LINK_CONSTANT),  input);
    MAKE_ARCH_PREF(io,    SYM(OUTPUT_LINK_CONSTANT), output);
}

void
agent_init(struct agent* agent)
{
    if (! constants_initialized) {
        /* Do one-time initialization */
        int i;

        constants_initialized = 1;

        for (i = NCONSTANTS - 1; i >= 1; --i)
            MAKE_SYMBOL(constants[i], symbol_type_symbolic_constant, i);
    }

    rete_init(agent);
    wmem_init(agent);

    agent->goals = agent->impasses = 0;

    agent_reset(agent);
}

void
agent_finish(struct agent* agent)
{
    while (agent->goals)
        pop_goal_id(agent);

    wmem_finish(agent);
    rete_finish(agent);
}

void
agent_reset(struct agent* agent)
{
    /* Clear working memory _before_ popping the goal stack so that
       our WME removals can propagate correctly through the RETE
       network. */
    wmem_clear(agent);

    while (agent->goals)
        pop_goal_id(agent);

    agent->next_available_identifier = 1;
    init_top_state(agent);
}

symbol_t
agent_get_identifier(struct agent* agent)
{
    symbol_t result;

    /* XXX once we run out of identifiers, we need to `gc' working
       memory to identify ranges of free identifiers from which to
       allocate ids. */
    ASSERT(agent->next_available_identifier < (1 << SYMBOL_VALUE_BITS),
           ("ran out of identifiers"));

    MAKE_SYMBOL(result, symbol_type_identifier, agent->next_available_identifier++);
    return result;
}

void
agent_elaborate(struct agent* agent)
{
    ASSERT(agent->goals != 0, ("no top-state"));
    wmem_elaborate(agent);
}

void
agent_operator_no_change(struct agent* agent, symbol_t goal)
{
    symbol_t state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-no-change => [%d]\n", state.val);
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(NONE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(NO_CHANGE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
}


void
agent_state_no_change(struct agent* agent, symbol_t goal)
{
    symbol_t state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("state-no-change => [%d]\n", state.val);
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(CHOICES_CONSTANT),    SYM(NONE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(NO_CHANGE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
}

void
agent_operator_conflict(struct agent* agent, symbol_t goal, struct symbol_list* operators)
{
    symbol_t state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-conflict => [%d]\n", state.val);
#endif

    MAKE_ARCH_PREF(state, SYM(SUPERSTATE_CONSTANT), goal);
    MAKE_ARCH_PREF(state, SYM(TYPE_CONSTANT),       SYM(STATE_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(ATTRIBUTE_CONSTANT),  SYM(OPERATOR_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(IMPASSE_CONSTANT),    SYM(CONFLICT_CONSTANT));
    MAKE_ARCH_PREF(state, SYM(QUIESCENCE_CONSTANT), SYM(T_CONSTANT));

    for ( ; operators != 0; operators = operators->next)
        MAKE_ARCH_PREF(state, SYM(ITEM_CONSTANT), operators->symbol);
}

void
agent_operator_tie(struct agent* agent, symbol_t goal, struct symbol_list* operators)
{
    symbol_t state = agent_get_identifier(agent);

    push_goal_id(agent, state);

#ifdef DEBUG
    printf("operator-tie => [%d]\n", state.val);
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
