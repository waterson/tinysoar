#ifndef rete_h__
#define rete_h__

#include "types.h"
#include "pool.h"

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

struct test {
    test_type_t type;
    union {
        symbol_t referent;
        /* list* conjunctions; */
        /* list* disjunctions; */
    } data;
};

typedef enum condtion_type {
    condition_type_positive,
    condition_type_negative,
    condition_type_conjunctive_negation
} condition_type_t;

struct condition {
    condition_type_t type;

    union {
        struct {
            struct test id_test;
            struct test attr_test;
            struct test value_test;
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

struct beta_node;

struct right_memory;

struct alpha_node {
    symbol_t id;
    symbol_t attr;
    symbol_t value;
    struct alpha_node*   siblings;
    struct beta_node*    children;
    struct right_memory* right_memories;
};

struct right_memory {
    struct wme* wme;
    struct right_memory* next_in_bucket;
    struct right_memory* next_in_alpha_node;
};

typedef enum field {
    field_id,
    field_attr,
    field_value
} field_t;

struct variable_binding {
    unsigned depth : BITS_PER_WORD - 2;
    field_t  field : 2;
};

struct beta_test {
    test_type_t type  : BITS_PER_WORD - 2;
    field_t     field : 2;
    union {
        struct variable_binding variable_referent;
        symbol_t constant_referent;
        /* list* disjunction_list; */
    } data;
    struct beta_test* next;
};

typedef enum beta_node_type {
    beta_node_type_root,
    beta_node_type_memory,
    beta_node_type_positive_join,
    beta_node_type_memory_positive_join,
    beta_node_type_negative,
    beta_node_type_conjunctive_negative,
    beta_node_type_conjunctive_negative_partner,
    beta_node_type_production
} beta_node_type_t;

struct beta_node {
    beta_node_type_t type;

    struct beta_node* parent;
    struct beta_node* siblings;
    struct beta_node* children;

    union {
        struct beta_test*  tests;      /* if positive_join */
        struct production* production; /* if production */
    } data;
};

struct token {
    struct token* parent;
    struct beta_node* node;
    struct wme* wme;
};

struct rete {
    struct beta_node root;
    struct pool beta_node_pool;
    struct pool beta_test_pool;
    struct pool variable_binding_list_pool;
};

extern void
rete_init(struct rete* net);

extern void
rete_add_production(struct rete* net, const struct production* p);

#endif /* rete_h__ */
