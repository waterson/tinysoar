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

#define DECLARE_NIL_SYMBOL \
  { 0, 0 }

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

#define SYMBOL_TO_WORD(v, t)    (((t) << SYMBOL_TYPE_SHIFT) | (v))

/* Predefined symbols used by the architecture */
#define ATTRIBUTE_CONSTANT      1
#define CHOICES_CONSTANT        2
#define CONFLICT_CONSTANT       3
#define IMPASSE_CONSTANT        4
#define INPUT_LINK_CONSTANT     5
#define IO_CONSTANT             6
#define ITEM_CONSTANT           7
#define MULTIPLE_CONSTANT       8
#define NIL_CONSTANT            9
#define NONE_CONSTANT          10
#define NO_CHANGE_CONSTANT     11
#define OPERATOR_CONSTANT      12
#define OUTPUT_LINK_CONSTANT   13
#define QUIESCENCE_CONSTANT    14
#define STATE_CONSTANT         15
#define SUPERSTATE_CONSTANT    16
#define TIE_CONSTANT           17
#define TYPE_CONSTANT          18
#define T_CONSTANT             19

#define NCONSTANTS             (T_CONSTANT + 1)
#define USER_CONSTANT_BASE     NCONSTANTS

extern symbol_t constants[NCONSTANTS];
#define SYM(x) constants[(x)]

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

#define VARIABLE_BINDING_TO_WORD(f, d) (((f) << BINDING_FIELD_SHIFT) | (d))

/* ---------------------------------------------------------------------- */

/*
 * Preferences
 */

#define SUPPORT_TYPE_BITS    2
#define PREFERENCE_TYPE_BITS (BITS_PER_WORD - SUPPORT_TYPE_BITS)

typedef enum preference_type {
    preference_type_unary              = 0,
    preference_type_binary             = 0 - (1 << (PREFERENCE_TYPE_BITS - 1)),

    preference_type_acceptable         = preference_type_unary + 0,
    preference_type_reject             = preference_type_unary + 1,
    preference_type_reconsider         = preference_type_unary + 2,
    preference_type_unary_indifferent  = preference_type_unary + 3,
    preference_type_best               = preference_type_unary + 4,
    preference_type_worst              = preference_type_unary + 5,
    preference_type_prohibit           = preference_type_unary + 6,
    preference_type_require            = preference_type_unary + 7,

    preference_type_binary_indifferent = preference_type_binary + 0,
    preference_type_better             = preference_type_binary + 1,
    preference_type_worse              = preference_type_binary + 2
} preference_type_t;

typedef enum support_type {
    support_type_isupport     = 0,
    support_type_osupport     = 1,
    support_type_architecture = -1,
    support_type_unknown      = -2
} support_type_t;

struct preference {
    struct slot*       slot;
    struct preference* next_in_slot;
    struct preference* next_in_instantiation;
    struct preference* prev_in_instantiation; /* XXX should be a back-pointer to the instantiation */
    preference_type_t  type    : PREFERENCE_TYPE_BITS;
    support_type_t     support : SUPPORT_TYPE_BITS;
    symbol_t           value;
    symbol_t           referent;
};

struct instantiation {
    struct production*    production;
    struct token*         token;
    struct preference     preferences;
    struct instantiation* next;
};

/* ---------------------------------------------------------------------- */

/*
 * Working Memory
 */

struct slot {
    symbol_t           id;
    symbol_t           attr;
    struct preference* preferences;
    struct wme*        wmes;
};

typedef enum wme_type {
    wme_type_normal,
    wme_type_acceptable
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

struct test {
    test_type_t type;
    union {
        symbol_t          referent;
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
    condition_type_positive = 0,
    condition_type_negative = 1,
    condition_type_conjunctive_negation = -2
} condition_type_t;

#define CONDITION_TYPE_BITS   2
#define CONDITION_TYPE_SHIFT  (BITS_PER_WORD - CONDITION_TYPE_BITS)
#define ACCEPTABLE_BITS       1
#define ACCEPTABLE_SHIFT      (CONDITION_TYPE_SHIFT - ACCEPTABLE_BITS)

struct condition {
    condition_type_t type       : CONDITION_TYPE_BITS;
    bool_t           acceptable : ACCEPTABLE_BITS;

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
        unsigned           unbound_variable;
        symbol_t           symbol;
        variable_binding_t variable_binding;
    } val;
};

/*
 * Actions
 */

struct action {
    struct action*    next;
    preference_type_t preference_type;
    struct rhs_value  id;
    struct rhs_value  attr;
    struct rhs_value  value;
    struct rhs_value  referent;
};

/*
 * Productions
 */
struct production {
#ifdef DEBUG
    char*                 name;
#endif
    struct condition*     conditions;
    struct action*        actions;
    struct instantiation* instantiations;
    unsigned              num_unbound_vars;
    support_type_t        support;
};

/*----------------------------------------------------------------------*/

/*
 * RETE
 */

/*
 * Alpha Memory
 */

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

/* XXX using a -1 here will generate a warning, but guarantees correct
   sign extension when we assign to wider fields. */
typedef enum relational_type {
    relational_type_constant = 0,
    relational_type_variable = -1
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
        unsigned           raw;
        symbol_t           constant_referent;
        variable_binding_t variable_referent;
        struct beta_test*  disjuncts;
    } data;
    struct beta_test* next;
};

enum beta_node_type_bits {
    beta_node_type_bit_hashed       = 0x01, /* XXX unused */
    beta_node_type_bit_memory       = 0x02,
    beta_node_type_bit_positive     = 0x04,
    beta_node_type_bit_negative     = 0x08,
    beta_node_type_bit_bottom_split = 0x10, /* XXX unused */
    beta_node_type_bit_special      = 0x40  /* XXX unused */
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
    /* XXX not needed for positive_join; could production nodes store
       this in the instantiation? */
    struct token* tokens;

    /* For negative nodes, the tokens that match the node and
       are ``blocked'' from propagating through. */
    /* XXX only needed by negative */
    struct token* blocked;

    union {
        /* if type in { positive_join, negative }, the tests to apply at this node */
        struct beta_test*  tests;

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

    /* The next token in the list of tokens owned by the beta node */
    struct token* next;

    /* The token that this token extends */
    struct token* parent;

    /* The wme that the token stands for */
    struct wme* wme;
};


struct match {
    struct production* production;
    union {
        struct token*         token;         /* for assertions */
        struct instantiation* instantiation; /* for retractions */
    } data;
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

    /* For the RETE network */
    struct beta_node*   root_node;
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

/*
 * Initialize the network
 */
extern void
rete_init(struct agent* agent);

#ifdef CONF_SOAR_RETE_CREATE
extern void
rete_create(struct agent* agent);
#endif

extern void
rete_finish(struct agent* agent);

#ifdef DEBUG
struct symtab;

extern void
rete_dump(struct agent* agent, struct symtab* symtab);
#endif

/*
 * Add a production to the network
 */
extern void
rete_add_production(struct agent* agent, struct production* p);

/*
 * Notify the network that a new working memory element has been
 * added or removed.
 */

typedef enum wme_operation {
    wme_operation_add,
    wme_operation_remove
} wme_operation_t;

extern void
rete_operate_wme(struct agent* agent, struct wme* wme, wme_operation_t op);

extern void
agent_init(struct agent* agent);

extern void
agent_reset(struct agent* agent);

extern void
agent_finish(struct agent* agent);

extern void
agent_elaborate(struct agent* agent);

extern void
agent_state_no_change(struct agent* agnet, symbol_t goal);

extern void
agent_operator_no_change(struct agent* agent, symbol_t goal);

extern void
agent_operator_conflict(struct agent* agent, symbol_t goal, struct symbol_list* operators);

extern void
agent_operator_tie(struct agent* agent, symbol_t goal, struct symbol_list* operators);

extern void
rete_push_goal_id(struct agent* agent, symbol_t goal_id);

extern symbol_t
rete_pop_goal_id(struct agent* agent);

extern symbol_t
rete_get_variable_binding(variable_binding_t binding, struct token* token);

extern void
wmem_init(struct agent* agent);

extern void
wmem_finish(struct agent* agent);

extern void
wmem_clear(struct agent* agent);

extern struct preference*
wmem_add_preference(struct agent* agent,
                    symbol_t id, symbol_t attr, symbol_t value,
                    preference_type_t type,
                    support_type_t support);

extern void
wmem_remove_preference(struct agent* agent, struct preference* pref);

extern struct preference*
wmem_get_preferences(struct agent* agent, symbol_t id, symbol_t attr);

extern struct wme*
wmem_get_wmes(struct agent* agent, symbol_t id, symbol_t attr);

extern void
wmem_elaborate(struct agent* agent);

typedef void (*wme_enumerator_t)(struct agent* agent, struct wme* wme, void* closure);

extern void
wmem_enumerate_wmes(struct agent* agent, wme_enumerator_t enumerator, void* closure);

extern symbol_t
agent_get_identifier(struct agent* agent);

#endif /* soar_h__ */
