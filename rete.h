#ifndef rete_h__
#define rete_h__

#include "types.h"

typedef enum condtion_type {
    condition_type_positive,
    condition_type_negative,
    condition_type_conjunctive_negation
} condition_type_t;

typedef enum test_type {
    test_type_blank,
    test_type_equality,
    test_type_not_equal,
    test_type_less,
    test_type_greater,
    test_type_less_or_equal,
    test_type_greater_or_equal,
    test_type_same_type,
    test_type_disjunction,
    test_type_conjunctive,
    test_type_goal_id,
    test_type_impasse_id,
} test_type_t;

struct condition {
    condition_type_t type;

    union {
        struct {
            test_type_t id_test;
            test_type_t attr_test;
            test_type_t value_test;
        } simple;
    } data;

    struct condition* next;
};

struct action {
    struct action* next;
};

struct production {
    struct condition* lhs;
    struct action*    rhs;
};

struct rete;

extern void
rete_add_production(struct rete* net, struct production* p);

#endif /* rete_h__ */
