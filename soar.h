#ifndef soar_h__
#define soar_h__

#include "config.h"
#include "ht.h"

/* ---------------------------------------------------------------------- */

/*
 * Symbols
 */
#define SYMBOL_TYPE_BITS   2
#define SYMBOL_TYPE_SHIFT  (BITS_PER_WORD - SYMBOL_TYPE_BITS)
#define SYMBOL_VALUE_BITS  SYMBOL_TYPE_SHIFT

typedef enum symbol_type {
    symbol_type_identifier           =  0,
    symbol_type_variable             =  1,
    symbol_type_symbolic_constant    = -2,
    symbol_type_integer_constant     = -1
} symbol_type_t;

typedef struct symbol {
    int           val  : SYMBOL_VALUE_BITS;
    symbol_type_t type : SYMBOL_TYPE_BITS;
} symbol_t;

#define GET_SYMBOL_VALUE(s)    ((s).val)
#define SET_SYMBOL_VALUE(s, v) ((s).val = (v))
#define GET_SYMBOL_TYPE(s)     ((s).type)
#define SET_SYMBOL_TYPE(s, t)  ((s).type = (t))

#define DECLARE_SYMBOL(v, t) \
  { (v), (t) }

#define MAKE_SYMBOL(s, t, v)     \
  BEGIN_MACRO                    \
  (s).type = (t); (s).val = (v); \
  END_MACRO

#define CLEAR_SYMBOL(s)     \
   BEGIN_MACRO              \
   *((unsigned*) &(s)) = 0; \
   END_MACRO

#define SYMBOLS_ARE_EQUAL(l, r) (*((unsigned*) &(l)) == *((unsigned*) &(r)))

#define SYMBOL_IS_NIL(s)        (*((unsigned*) &(s)) == 0)

/* Predefined symbols used by the architecture */
#define ATTRIBUTE_CONSTANT      1
#define CHOICES_CONSTANT        2
#define INPUT_LINK_CONSTANT     3
#define IO_CONSTANT             4
#define NIL_CONSTANT            5
#define NONE_CONSTANT           6
#define NO_CHANGE_CONSTANT      7
#define OPERATOR_CONSTANT       8
#define OUTPUT_LINK_CONSTANT    9
#define QUIESCENCE_CONSTANT    10
#define STATE_CONSTANT         11
#define SUPERSTATE_CONSTANT    12
#define TIE_CONSTANT           13
#define TYPE_CONSTANT          14
#define T_CONSTANT             15

#define USER_CONSTANT_BASE     16

struct symbol_list {
    symbol_t symbol;
    struct symbol_list* next;
};

/* ---------------------------------------------------------------------- */

/*
 * Variables
 */

#define BINDING_FIELD_BITS   2
#define BINDING_FIELD_SHIFT  (BITS_PER_WORD - BINDING_FIELD_BITS) 
#define BINDING_DEPTH_BITS   BINDING_FIELD_SHIFT

typedef enum field {
    field_id    =  0,
    field_attr  =  1,
    field_value = -2
} field_t;

typedef struct variable_binding {
    unsigned depth : BINDING_DEPTH_BITS;
    field_t  field : BINDING_FIELD_BITS;
} variable_binding_t;

#define VARIABLE_BINDINGS_ARE_EQUAL(l, r)  (*((unsigned*)&(l)) == *((unsigned*)&(r)))



/* ---------------------------------------------------------------------- */

/*
 * Preferences
 */

typedef enum preference_type {
    preference_type_unary              =  0,
    preference_type_binary             = -1,

    preference_type_acceptable         = preference_type_unary + 0,
    preference_type_reject             = preference_type_unary + 1,
    preference_type_reconsider         = preference_type_unary + 2,
    preference_type_unary_indifferent  = preference_type_unary + 3,
    preference_type_best               = preference_type_unary + 4,
    preference_type_worst              = preference_type_unary + 5,
    preference_type_prohibit           = preference_type_unary + 6,
    preference_type_require            = preference_type_unary + 7,

    preference_type_binary_indifferent = preference_type_binary - 0,
    preference_type_better             = preference_type_binary - 1,
    preference_type_worse              = preference_type_binary - 2
} preference_type_t;

#define SUPPORT_TYPE_BITS    2
#define PREFERENCE_TYPE_BITS (BITS_PER_WORD - SUPPORT_TYPE_BITS)


typedef enum support_type {
    support_type_isupport = 0,
    support_type_osupport = 1,
    support_type_unknown  = -1
} support_type_t;

struct preference {
    struct preference* next_in_instantiation;
    struct preference* next_in_slot;
    preference_type_t  type    : PREFERENCE_TYPE_BITS;
    support_type_t     support : SUPPORT_TYPE_BITS;
    symbol_t           id;
    symbol_t           attr;
    symbol_t           value;
    symbol_t           referent;
};

struct instantiation {
    struct instantiation* next;
    struct production*    production;
    struct token*         token;
    struct preference*    preferences;
};

/* ---------------------------------------------------------------------- */

/*
 * Working Memory
 */

struct wme;

struct slot {
    symbol_t           id;
    symbol_t           attr;
    struct preference* preferences;
    struct wme*        wmes;
};

typedef enum wme_type {
    wme_type_normal,
    wme_type_acceptable_preference
} wme_type_t;

struct wme {
    struct slot* slot;
    symbol_t     value;
    wme_type_t   type;
    struct wme*  next;
};

struct slot_list {
    struct slot*      slot;
    struct slot_list* next;
};

/* ---------------------------------------------------------------------- */

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
 * RHS values
 */
typedef enum rhs_value_type {
    rhs_value_type_symbol,
    rhs_value_type_variable_binding,
    rhs_value_type_unbound_variable
} rhs_value_type_t;

struct rhs_value {
    rhs_value_type_t type;
    union {
        symbol_t           symbol;
        variable_binding_t variable_binding;
        unsigned           unbound_variable;
    } val;
};

/*
 * Actions
 */

struct action {
    struct action*    next;
    preference_type_t preference_type : PREFERENCE_TYPE_BITS;
    support_type_t    support_type    : SUPPORT_TYPE_BITS;
    struct rhs_value  id;
    struct rhs_value  attr;
    struct rhs_value  value;
    struct rhs_value  referent;
};

/*
 * Productions
 */
struct production {
    struct condition*     conditions;
    struct action*        actions;
    struct instantiation* instantiations;
    unsigned              num_unbound_vars;
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
    struct right_memory* next_in_alpha_node;
};

/*
 * Beta Memory
 */

typedef enum relational_type {
    relational_type_constant, /* XXX sign extension? */
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

    /* Beta network structure */
    struct beta_node* parent;
    struct beta_node* siblings;
    struct beta_node* children;

    /* Back-pointer to the alpha node that this beta node is
       `attached' to */
    struct alpha_node* alpha_node;

    /* List of beta nodes that share the alpha node */
    struct beta_node* next_with_same_alpha_node;

    /* The tokens that exist at this beta node */
    struct token* tokens;

    union {
        /* if type == positive_join, the tests to apply at this node */
        struct beta_test*        tests;

        /* if type == production, the the production that's been matched */
        struct production* production;
    } data;
};

/*
 * Tokens
 */
struct token {
    /* Back-pointer to the beta node that owns the token */
    struct beta_node* node;

    /* The next token in the list of tokens owned by the parent beta node */
    struct token* next;

    /* The token that this token extends */
    struct token* parent;

    /* The wme that the token stands for */
    struct wme* wme;
};


struct match {
    struct production* production;
    struct token* token;
    struct match* next;
};

/* ---------------------------------------------------------------------- */

/*
 * An `agent', which is everything that's needed to maintain the state
 * of a Soar process.
 */
struct agent {
    /* For the symbol table */
    unsigned next_available_identifier;
    symbol_t operator_symbol;

    /* For the RETE network */
    struct beta_node    root_node;
    struct token        root_token;
    struct alpha_node*  alpha_nodes[16];
    struct symbol_list* goals;
    struct symbol_list* impasses;
    struct match*       assertions;
    struct match*       retractions;

    /* For working memory */
    struct ht         slots;
    struct slot_list* modified_slots;
};

/* ---------------------------------------------------------------------- */

void
pref_process_matches(struct agent* agent);

struct preference*
pref_create_preference(symbol_t id, symbol_t attr, symbol_t value,
                       preference_type_t type,
                       support_type_t support);

/*
 * Initialize the network
 */
extern void
rete_init(struct agent* agent);

/*
 * Add a production to the network
 */
extern void
rete_add_production(struct agent* agent, struct production* p);

/*
 * Notify the network that a new working memory element has been
 * added.
 */
extern void
rete_add_wme(struct agent* agent, struct wme* wme);

extern void
rete_remove_wme(struct agent* agent, struct wme* wme);

extern void
agent_init(struct agent* agent);

extern void
rete_push_goal_id(struct agent* agent, symbol_t goal_id);

extern symbol_t
rete_pop_goal_id(struct agent* agent);

extern symbol_t
rete_get_variable_binding(variable_binding_t binding, struct token* token);

extern void
wmem_init(struct agent* agent);

extern void
wmem_add_preference(struct agent* agent, struct preference* pref);

extern void
wmem_decide(struct agent* agent);

typedef void (*wme_enumerator_t)(struct agent* agent, struct wme* wme, void* closure);

extern void
wmem_enumerate_wmes(struct agent* agent, wme_enumerator_t enumerator, void* closure);

extern symbol_t
agent_get_identifier(struct agent* agent);

#endif /* soar_h__ */