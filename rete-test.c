#include "wmem.h"
#include "rete.h"

struct test_list complex_tests[] = {
};

struct test_list test_list[2];
struct condition conditions[2];
struct production productions[1];

static void
add_productions(struct rete* net)
{
    productions[0].lhs = &conditions[0];
    productions[0].rhs = 0; /*XXX*/

    conditions[0].type = condition_type_positive;
    conditions[0].data.simple.id_test.type = test_type_conjunctive;
    conditions[0].data.simple.id_test.data.conjuncts = &test_list[0];

    test_list[0].test.type = test_type_goal_id;
    test_list[0].next = &test_list[1];

    test_list[1].test.type = test_type_equality;
    MAKE_SYMBOL(test_list[1].test.data.referent, symbol_type_variable, 1); /* <s> */
    test_list[1].next = 0;

    conditions[0].data.simple.attr_test.type = test_type_equality;
    MAKE_SYMBOL(conditions[0].data.simple.attr_test.data.referent, symbol_type_symbolic_constant, 1); /* ^superstate */

    conditions[0].data.simple.value_test.type = test_type_equality;
    MAKE_SYMBOL(conditions[0].data.simple.value_test.data.referent, symbol_type_symbolic_constant, 1); /* nil */

    conditions[0].next = 0;

    rete_add_production(net, &productions[0]);
}

int
main(int argc, char* argv[])
{
    struct working_memory wmem;
    struct rete net;
    working_memory_init(&wmem);
    rete_init(&net, &wmem);
    add_productions(&net);
    return 0;
}
