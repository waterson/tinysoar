#include "wmem.h"
#include "rete.h"

struct test_list test_list[2];
struct condition conditions[2];
struct production productions[1];

static symbol_t symtab[] = {
    DECLARE_SYMBOL(1, symbol_type_variable),

    DECLARE_SYMBOL(1, symbol_type_identifier),

    DECLARE_SYMBOL(1, symbol_type_symbolic_constant),
    DECLARE_SYMBOL(2, symbol_type_symbolic_constant),
};

#define VARIABLE_BASE        0
#define VARIABLE_STATE       VARIABLE_BASE + 0

#define IDENTIFIER_BASE      1
#define IDENTIFIER_S1        IDENTIFIER_BASE + 0

#define CONSTANT_BASE        2
#define CONSTANT_SUPERSTATE  CONSTANT_BASE + 0
#define CONSTANT_NIL         CONSTANT_BASE + 1

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
    test_list[1].test.data.referent = symtab[VARIABLE_STATE];
    test_list[1].next = 0;

    conditions[0].data.simple.attr_test.type = test_type_equality;
    conditions[0].data.simple.attr_test.data.referent = symtab[CONSTANT_SUPERSTATE];

    conditions[0].data.simple.value_test.type = test_type_equality;
    conditions[0].data.simple.value_test.data.referent = symtab[CONSTANT_NIL];

    conditions[0].next = 0;

    rete_add_production(net, &productions[0]);
}

struct wmem wmem;
struct rete net;

int
main(int argc, char* argv[])
{
    struct wme* wme;

    wmem_init(&wmem);
    rete_init(&net, &wmem);
    add_productions(&net);

    rete_push_goal_id(&net, symtab[IDENTIFIER_S1]);

    wme = wmem_add(&wmem, symtab[IDENTIFIER_S1], symtab[CONSTANT_SUPERSTATE], symtab[CONSTANT_NIL], wme_type_normal);
    rete_add_wme(&net, wme);
    return 0;
}
