#ifndef parser_h__
#define parser_h__

#include "soar.h"

struct attr_value_test {
    struct test attr_test;
    struct test value_test;
};

struct attr_value_test_list {
    struct attr_value_test tests;
    struct attr_value_test_list* next;
};



extern struct production*
soar_parse_rule(const char* rule);

#endif /* parser_h__ */
