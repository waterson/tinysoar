#include "rete.h"

#define VAR_S  1
#define VAR_O1 2
#define VAR_O2 3

#define SYM_NIL         1
#define SYM_WAIT        2
#define SYM_SUPERSTATE  3
#define SYM_OPERATOR    4
#define SYM_NAME        5
#define SYM_STATE       6
#define SYM_TYPE        7
#define SYM_IO          8
#define SYM_INPUT_LINK  9
#define SYM_OUTPUT_LINK 10

#define OBJECT_IO          2
#define OBJECT_INPUT_LINK  3
#define OBJECT_OUTPUT_LINK 4

instantiation_node_t instantiations[] = {
    { { node_type_instantiation }, "wait*propose" },
    { { node_type_instantiation }, "wait*done" },
};

join_node_t joins[] = {
    { { { node_type_join, 0 }, (node_t*)(instantiations + 1) }, VAR_O1, VAR_O2 }
};

simple_test_node_t simple_tests[] = {
    { { { node_type_simple_test, (node_t*)(simple_tests + 1) }, (node_t*)(instantiations + 0) },
      VAR_S, SYM_SUPERSTATE, MAKE_SYMBOL_VALUE(SYM_NIL) },

    { { { node_type_simple_test, 0 }, (node_t*)(joins + 0) },
      VAR_S, SYM_OPERATOR, MAKE_VARIABLE_VALUE(VAR_O1) },

    { { { node_type_simple_test, 0 }, (node_t*)(joins + 0) },
      VAR_O2, SYM_NAME, MAKE_SYMBOL_VALUE(SYM_WAIT) }
};

complex_test_node_t complex_tests[] = {
    { { { node_type_complex_test, (node_t*)(simple_tests + 2), }, (node_t*)(simple_tests + 0) },
      complex_test_type_state_object, VAR_S },
};

inner_node_t* root = (inner_node_t*) complex_tests;

wme_t wmes[] = {
    { OBJECT_STATE, SYM_SUPERSTATE,  MAKE_SYMBOL_VALUE(SYM_NIL) },
    { OBJECT_STATE, SYM_TYPE,        MAKE_SYMBOL_VALUE(SYM_STATE) },
    { OBJECT_STATE, SYM_IO,          MAKE_OBJECT_VALUE(OBJECT_IO) },
    { OBJECT_IO,    SYM_INPUT_LINK,  MAKE_OBJECT_VALUE(OBJECT_INPUT_LINK) },
    { OBJECT_IO,    SYM_OUTPUT_LINK, MAKE_OBJECT_VALUE(OBJECT_OUTPUT_LINK) },
    { 0 }
};

int
main(int argc, char* argv[])
{
    rete_t rete = { root };
    rete_init(&rete, wmes);
    return 0;
}
