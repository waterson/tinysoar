#include "soar.h"

struct test_list  test_list[2];
struct condition  conditions[2];
struct action     actions[2];
struct production productions[1];

static symbol_t symbols[] = {
    DECLARE_SYMBOL(1, symbol_type_variable),
    DECLARE_SYMBOL(2, symbol_type_variable),

    DECLARE_SYMBOL(0, 0),
    DECLARE_SYMBOL(0, 0),

    DECLARE_SYMBOL(USER_CONSTANT_BASE + 0, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(USER_CONSTANT_BASE + 1, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(USER_CONSTANT_BASE + 2, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(USER_CONSTANT_BASE + 3, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(USER_CONSTANT_BASE + 4, symbol_type_symbolic_constant),
};

#define VARIABLE_BASE        0
#define VARIABLE_STATE       VARIABLE_BASE + 0
#define VARIABLE_INPUT_LINK  VARIABLE_BASE + 1

#define IDENTIFIER_BASE      2
#define IDENTIFIER_S1        IDENTIFIER_BASE + 0
#define IDENTIFIER_I1        IDENTIFIER_BASE + 1

#define CONSTANT_BASE        4
#define CONSTANT_SUPERSTATE  CONSTANT_BASE + 0
#define CONSTANT_NIL         CONSTANT_BASE + 1
#define CONSTANT_INPUT_LINK  CONSTANT_BASE + 2
#define CONSTANT_NAME        CONSTANT_BASE + 3
#define CONSTANT_WAIT        CONSTANT_BASE + 4

static void
init_symbols(struct agent* agent)
{
    int i;

    for (i = IDENTIFIER_BASE; i < CONSTANT_BASE; ++i)
        symbols[i] = symtab_get_identifier(agent);
}

static void
init_productions(struct agent* agent)
{
    productions[0].conditions = &conditions[0];
    productions[0].actions    = &actions[0];
    productions[0].num_unbound_vars = 1;

    /* (<s> ^superstate nil) */
    conditions[0].type = condition_type_positive;
    conditions[0].data.simple.id_test.type = test_type_conjunctive;
    conditions[0].data.simple.id_test.data.conjuncts = &test_list[0];

    test_list[0].test.type = test_type_goal_id;
    test_list[0].next = &test_list[1];

    test_list[1].test.type = test_type_equality;
    test_list[1].test.data.referent = symbols[VARIABLE_STATE];
    test_list[1].next = 0;

    conditions[0].data.simple.attr_test.type = test_type_equality;
    conditions[0].data.simple.attr_test.data.referent = symbols[CONSTANT_SUPERSTATE];

    conditions[0].data.simple.value_test.type = test_type_equality;
    conditions[0].data.simple.value_test.data.referent = symbols[CONSTANT_NIL];

    conditions[0].next = &conditions[1];

    /* (<s> ^input-link <i1>) */
    conditions[1].type = condition_type_positive;
    conditions[1].data.simple.id_test.type = test_type_equality;
    conditions[1].data.simple.id_test.data.referent = symbols[VARIABLE_STATE];
    conditions[1].data.simple.attr_test.type = test_type_equality;
    conditions[1].data.simple.attr_test.data.referent = symbols[CONSTANT_INPUT_LINK];
    conditions[1].data.simple.value_test.type = test_type_equality;
    conditions[1].data.simple.value_test.data.referent = symbols[VARIABLE_INPUT_LINK];
    conditions[1].next = 0;

    /* (<s> ^operator <o> +) */
    actions[0].next = &actions[1];
    actions[0].preference_type = preference_type_acceptable;
    actions[0].support_type    = support_type_isupport;

    actions[0].id.type = rhs_value_type_symbol;
    actions[0].id.val.symbol = symbols[VARIABLE_STATE];

    actions[0].attr.type = rhs_value_type_symbol;
    actions[0].attr.val.symbol = agent->operator_symbol;

    actions[0].value.type = rhs_value_type_unbound_variable;
    actions[0].value.val.unbound_variable = 0;

    /* (<o> ^name wait) */
    actions[1].next = 0;
    actions[1].preference_type = preference_type_acceptable;
    actions[1].support_type    = support_type_isupport;

    actions[1].id.type = rhs_value_type_unbound_variable;
    actions[1].id.val.unbound_variable = 0;

    actions[1].attr.type = rhs_value_type_symbol;
    actions[1].attr.val.symbol = symbols[CONSTANT_NAME];

    actions[1].value.type = rhs_value_type_symbol;
    actions[1].value.val.symbol = symbols[CONSTANT_WAIT];
}

struct agent agent;

int
main(int argc, char* argv[])
{
    struct wme* wme;

    agent_init(&agent);

    init_symbols(&agent);
    init_productions(&agent);
    rete_add_production(&agent, &productions[0]);

    rete_push_goal_id(&agent, symbols[IDENTIFIER_S1]);

    wme = wmem_add(&agent, symbols[IDENTIFIER_S1], symbols[CONSTANT_SUPERSTATE], symbols[CONSTANT_NIL], wme_type_normal);
    rete_add_wme(&agent, wme);

    wme = wmem_add(&agent, symbols[IDENTIFIER_S1], symbols[CONSTANT_INPUT_LINK], symbols[IDENTIFIER_I1], wme_type_normal);
    rete_add_wme(&agent, wme);

    pref_process_matches(&agent);

    return 0;
}
