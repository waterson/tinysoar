#ifndef rete_h__
#define rete_h__

#include "types.h"
#include "pool.h"

/*----------------------------------------------------------------------*/

/*
 * Productions
 */

/*
 * Tests.
 */
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

struct test_list;

struct test {
    test_type_t type;
    union {
        symbol_t referent;
        struct test_list* conjuncts;
        struct test_list* disjuncts;
    } data;
};

struct test_list {
    struct test test;
    struct test_list* next;
};

/*
 * Conditions
 */
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

/*
 * Actions
 */
struct action {
    struct action* next;
};

/*
 * Productions
 */
struct production {
    struct condition* lhs;
    struct action*    rhs;
};

/*----------------------------------------------------------------------*/

/*
 * RETE
 */
struct beta_node;

/*
 * Alpha Memory
 */
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

/*
 * Variables
 */

#define BINDING_FIELD_BITS   2
#define BINDING_FIELD_SHIFT  (BITS_PER_WORD - BINDING_FIELD_BITS) 
#define BINDING_DEPTH_BITS   BINDING_FIELD_SHIFT

typedef enum field {
    field_id,
    field_attr,
    field_value
} field_t;

typedef struct variable_binding {
    unsigned depth : BINDING_DEPTH_BITS;
    field_t  field : BINDING_FIELD_BITS;
} variable_binding_t;


/*
 * Beta Memory
 */

#define BETA_TEST_FIELD_BITS   2
#define BETA_TEST_FIELD_SHIFT  (BITS_PER_WORD - BETA_TEST_FIELD_BITS)
#define BETA_TEST_TYPE_BITS    BETA_TEST_FIELD_SHIFT

struct beta_test {
    test_type_t type  : BETA_TEST_TYPE_BITS;
    field_t     field : BETA_TEST_FIELD_BITS;
    union {
        variable_binding_t variable_referent;
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

/*
 * Tokens
 */
struct token {
    struct token* parent;
    struct beta_node* node;
    struct wme* wme;
};

/*
 * The Network
 */
struct rete {
    struct beta_node root;
    struct pool beta_node_pool;
    struct pool beta_test_pool;
    struct pool variable_binding_list_pool;
};

/*----------------------------------------------------------------------*/


/*
 * Initialize the network
 */
extern void
rete_init(struct rete* net);

/*
 * Add a production to the network
 */
extern void
rete_add_production(struct rete* net, const struct production* p);

#endif /* rete_h__ */
