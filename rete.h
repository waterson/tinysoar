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
    test_type_disjunctive,
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

typedef enum alpha_type {
    alpha_type_wme,
    alpha_type_acceptable_preference
} alpha_type_t;

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

#define VARIABLE_BINDINGS_ARE_EQUAL(l, r)  (*((unsigned*)&(l)) == *((unsigned*)&(r)))

/*
 * Beta Memory
 */

typedef enum relational_type {
    relational_type_constant,
    relational_type_variable
} relational_type_t;

#define BETA_TEST_FIELD_BITS   2
#define BETA_TEST_FIELD_SHIFT  (BITS_PER_WORD - BETA_TEST_FIELD_BITS)
#define RELATIONAL_TYPE_BITS   1
#define RELATIONAL_TYPE_SHIFT  (BETA_TEST_FIELD_SHIFT - RELATIONAL_TYPE_BITS)
#define BETA_TEST_TYPE_BITS    RELATIONAL_TYPE_SHIFT

struct beta_test {
    test_type_t       type            : BETA_TEST_TYPE_BITS;
    relational_type_t relational_type : RELATIONAL_TYPE_BITS;
    field_t           field           : BETA_TEST_FIELD_BITS;
    union {
        variable_binding_t variable_referent;
        symbol_t constant_referent;
        struct beta_test* disjuncts;
    } data;
    struct beta_test* next;
};

enum beta_node_type_bits {
    beta_node_type_bit_hashed       = 0x01,
    beta_node_type_bit_memory       = 0x02,
    beta_node_type_bit_positive     = 0x04,
    beta_node_type_bit_negative     = 0x08,
    beta_node_type_bit_bottom_split = 0x10,
    beta_node_type_bit_special      = 0x40
};

typedef enum beta_node_type {
    beta_node_type_memory
      = beta_node_type_bit_memory,

    beta_node_type_positive_join
      = beta_node_type_bit_positive,

    beta_node_type_memory_positive_join
      = beta_node_type_bit_memory | beta_node_type_bit_positive,

    beta_node_type_negative
      = beta_node_type_bit_negative,

    beta_node_type_root
      = beta_node_type_bit_special,

    beta_node_type_conjunctive_negative
      = beta_node_type_bit_special | beta_node_type_bit_hashed,

    beta_node_type_conjunctive_negative_partner
      = beta_node_type_bit_special | beta_node_type_bit_hashed | beta_node_type_bit_memory,

    beta_node_type_production
      = beta_node_type_bit_special | beta_node_type_bit_positive,

} beta_node_type_t;

struct beta_node {
    beta_node_type_t type;

    struct beta_node* parent;
    struct beta_node* siblings;
    struct beta_node* children;

    struct alpha_node* alpha_node;

    struct beta_node* next_with_same_alpha_node;

    union {
        struct beta_test*        tests;      /* if positive_join */
        const struct production* production; /* if production */
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
    struct beta_node   root_node;
    struct token       root_token;
    struct pool        alpha_node_pool;
    struct pool        beta_node_pool;
    struct pool        beta_test_pool;
    struct pool        variable_binding_list_pool;
    struct pool        token_pool;
    struct pool        goal_impasse_pool;
    struct alpha_node* alpha_nodes[16];
    struct symbol_list* goals;
    struct symbol_list* impasses;
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
