#include "rete.h"

struct condition conditions[] = {
    { condition_type_positive, { { { test_type_goal_id, { { 1, symbol_type_variable } } },
                                   { test_type_blank,   { { 0, symbol_type_identifier } } },
                                   { test_type_blank,   { { 0, symbol_type_identifier } } } } },
      &conditions[1] },

    { condition_type_positive, { { { test_type_equality, { { 1, symbol_type_variable } } },
                                   { test_type_equality, { { 2, symbol_type_symbolic_constant } } },
                                   { test_type_equality, { { 3, symbol_type_symbolic_constant } } } } },
      0 }                                   
};

struct production productions[] = {
    { &conditions[0], 0 }
};

int
main(int argc, char* argv[])
{
    struct rete net;
    rete_init(&net);
    rete_add_production(&net, &productions[0]);
    return 0;
}
