/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "MPL"); you may not use this file except in
 * compliance with the MPL.  You may obtain a copy of the MPL at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the MPL is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the MPL
 * for the specific language governing rights and limitations under the
 * MPL.
 *
 * The Initial Developer of this code under the MPL is Christopher
 * R. Waterson. Portions created by Christopher R. Waterson are
 * Copyright (C) 2000, 2001, 2002 Christopher R. Waterson. All Rights
 * Reserved.
 *
 * Contributor(s):
 *   Christopher R. Waterson <waterson@maubi.net>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or 
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 */

/*
 * Definitions common to most of the TinySoar code.
 */
#ifndef soar_h__
#define soar_h__

#include "config.h"
#include "ht.h"

/* ---------------------------------------------------------------------- */

/*
 * Symbols. The symbol's type is stored in the low two bits; the
 * symbol's value in the high bits.
 *
 * XXX might want to change symbol_type_integer_constant to zero if we
 * ever decide to implement RHS expressions: that way addition and
 * subtraction can be done without extracting the value.
 */
#define SYMBOL_TYPE_BITS   2
#define SYMBOL_VALUE_BITS  (BITS_PER_WORD - SYMBOL_TYPE_BITS)
#define SYMBOL_VALUE_SHIFT SYMBOL_TYPE_BITS
#define SYMBOL_VALUE_MASK  (((unsigned) (~0)) << SYMBOL_VALUE_SHIFT)
#define SYMBOL_TYPE_MASK   ~SYMBOL_VALUE_MASK

typedef enum symbol_type {
    symbol_type_identifier        =  0,
    symbol_type_variable          =  1,
    symbol_type_symbolic_constant =  2,
    symbol_type_integer_constant  =  3
} symbol_type_t;

typedef int symbol_t;

#define ASSERT_VALID_SYMBOL_TYPE(t) \
  ASSERT(((t) & ~SYMBOL_TYPE_MASK) == 0, ("bad symbol type %d", (t)))

#define SYMBOL_TO_WORD(t, v)    ((t) | ((v) << SYMBOL_VALUE_SHIFT))
#define GET_SYMBOL_VALUE(s)     ((s) >> SYMBOL_VALUE_SHIFT)
#define SET_SYMBOL_VALUE(s, v)  ((s) &= ~SYMBOL_VALUE_MASK, (s) |= ((v) << SYMBOL_VALUE_SHIFT))
#define GET_SYMBOL_TYPE(s)      ((s) & SYMBOL_TYPE_MASK)
#define SET_SYMBOL_TYPE(s, t)   (ASSERT_VALID_SYMBOL_TYPE(t), (s) &= ~SYMBOL_TYPE_MASK, (s) |= (t))
#define DECLARE_SYMBOL(t, v)    SYMBOL_TO_WORD((t), (v))
#define DECLARE_NIL_SYMBOL      0
#define INIT_SYMBOL(s, t, v)    (ASSERT_VALID_SYMBOL_TYPE(t), (s) = SYMBOL_TO_WORD((t), (v)))
#define CLEAR_SYMBOL(s)         ((s) = 0)
#define SYMBOLS_ARE_EQUAL(l, r) ((l) == (r))
#define SYMBOL_IS_NIL(s)        SYMBOLS_ARE_EQUAL((s), 0)

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
    struct symbol_list *next;
};

/* ---------------------------------------------------------------------- */

/*
 * Fields
 */

#define FIELD_BITS   2
#define FIELD_SHIFT  (BITS_PER_WORD - FIELD_BITS)
#define FIELD_MASK   (((unsigned) (~0)) << FIELD_SHIFT)

#define ASSERT_VALID_FIELD(f) \
    ASSERT(((f) & ~FIELD_MASK) == 0, ("bad field %d", (f)))

#define GET_FIELD(b) ((field_t)((b) & FIELD_MASK))
#define SET_FIELD(b, f) \
    (ASSERT_VALID_FIELD(f), (b) &= ~FIELD_MASK, (b) |= (f))

typedef enum field {
    field_id    = 0 << FIELD_SHIFT,
    field_attr  = 1 << FIELD_SHIFT,
    field_value = 2 << FIELD_SHIFT
} field_t;


/*
 * Variables
 */

typedef unsigned variable_binding_t;

#define BINDING_DEPTH_BITS   FIELD_SHIFT
#define BINDING_DEPTH_MASK   ~FIELD_MASK

#define VARIABLE_BINDING_TO_WORD(f, d)     ((f) | (d))
#define INIT_VARIABLE_BINDING(b, f, d)     ((b) = VARIABLE_BINDING_TO_WORD((f), (d)))
#define GET_VARIABLE_BINDING_FIELD(b)      GET_FIELD(b)
#define SET_VARIABLE_BINDING_FIELD(b, f)   SET_FIELD(b, f)
#define GET_VARIABLE_BINDING_DEPTH(b)      ((b) & BINDING_DEPTH_MASK)
#define SET_VARIABLE_BINDING_DEPTH(b, d)   ((b) &= ~BINDING_DEPTH_MASK, (b) |= (d) & BINDING_DEPTH_MASK)
#define VARIABLE_BINDINGS_ARE_EQUAL(l, r)  ((l) == (r))

/* ---------------------------------------------------------------------- */

/*
 * Preferences
 */

#define SUPPORT_TYPE_BITS     2
#define SUPPORT_TYPE_SHIFT    (BITS_PER_WORD - SUPPORT_TYPE_BITS)
#define SUPPORT_TYPE_MASK     (((unsigned) (~0)) << SUPPORT_TYPE_SHIFT)

#define PREFERENCE_STATE_BITS  1
#define PREFERENCE_STATE_SHIFT (SUPPORT_TYPE_SHIFT - PREFERENCE_STATE_BITS)
#define PREFERENCE_STATE_MASK  ((((unsigned) (~0)) << PREFERENCE_STATE_SHIFT) & ~SUPPORT_TYPE_MASK)

#define PREFERENCE_TYPE_BITS   (BITS_PER_WORD - SUPPORT_TYPE_BITS - PREFERENCE_STATE_BITS)
#define PREFERENCE_TYPE_MASK   (~(SUPPORT_TYPE_MASK | PREFERENCE_STATE_MASK))

typedef enum preference_type {
    preference_type_unary              = 0,
    preference_type_binary             = (1 << (PREFERENCE_TYPE_BITS - 1)),

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

typedef enum preference_state {
    preference_state_live   = 0 << PREFERENCE_STATE_SHIFT,
    preference_state_zombie = 1 << PREFERENCE_STATE_SHIFT
} preference_state_t;

typedef enum support_type {
    support_type_isupport     = 0 << SUPPORT_TYPE_SHIFT,
    support_type_osupport     = 1 << SUPPORT_TYPE_SHIFT,
    support_type_architecture = 2 << SUPPORT_TYPE_SHIFT,
    support_type_unknown      = 3 << SUPPORT_TYPE_SHIFT
} support_type_t;

struct preference {
    struct slot          *slot;
    struct preference    *next_in_slot;
    struct preference    *next_in_instantiation;
    struct instantiation *instantiation;
    unsigned              bits;
    symbol_t              value;
    symbol_t              referent;
};

#define ASSERT_VALID_PREFERENCE_TYPE(t) \
    ASSERT(((t) & ~PREFERENCE_TYPE_MASK) == 0, ("invalid preference type %d", (t)))

#define GET_PREFERENCE_TYPE(p)    ((p)->bits & PREFERENCE_TYPE_MASK)
#define SET_PREFERENCE_TYPE(p, t) (ASSERT_VALID_PREFERENCE_TYPE(t), (p)->bits &= ~PREFERENCE_TYPE_MASK, (p)->bits |= (t))

#define ASSERT_VALID_PREFERENCE_STATE(s) \
    ASSERT(((s) & ~PREFERENCE_STATE_MASK) == 0, ("invalid preference state %d", (s)))

#define GET_PREFERENCE_STATE(p)    ((p)->bits & PREFERENCE_STATE_MASK)
#define SET_PREFERENCE_STATE(p, s) (ASSERT_VALID_PREFERENCE_STATE(s), (p)->bits &= ~PREFERENCE_STATE_MASK, (p)->bits |= (s))

#define ASSERT_VALID_SUPPORT_TYPE(s) \
    ASSERT(((s) & ~SUPPORT_TYPE_MASK) == 0, ("invalid support type %d", (s)))

#define GET_PREFERENCE_SUPPORT_TYPE(p)    ((p)->bits & SUPPORT_TYPE_MASK)
#define SET_PREFERENCE_SUPPORT_TYPE(p, s) (ASSERT_VALID_SUPPORT_TYPE(s), (p)->bits &= ~SUPPORT_TYPE_MASK, (p)->bits |= (s))

struct instantiation {
    struct production    *production;
    struct token         *token;
    struct preference    *preferences;
    struct instantiation *next;
};

/* ---------------------------------------------------------------------- */

/*
 * Working Memory.
 */

struct slot {
    symbol_t           id;
    symbol_t           attr;
    struct preference *preferences;
    struct wme        *wmes;
};

typedef enum wme_type {
    wme_type_normal     =  0,
    wme_type_acceptable =  1 /* XXX needs to be -1 to work with MSVC. */
} wme_type_t;

typedef enum wme_state {
    wme_state_live   =  0,
    wme_state_zombie = -1
} wme_state_t;

struct wme {
    struct slot *slot;
    symbol_t     value;
    wme_type_t   type : 1;
    wme_state_t  state : 1;
    struct wme  *next;
};

struct slot_list {
    struct slot      *slot;
    struct slot_list *next;
};

/* ---------------------------------------------------------------------- */

/*
 * Productions
 */

/*
 * Tests. Note that we don't support `impasse' tests: we don't ever
 * allow attribute impasses to occur (and this appears to be the only
 * way that they can be created in Soar-8.3).
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
} test_type_t;

struct test {
    test_type_t type;
    union {
        symbol_t          referent;
        struct test_list *conjuncts;
        struct test_list *disjuncts;
    } data;
};

struct test_list {
    struct test test;
    struct test_list *next;
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

    struct condition *next;
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
    struct action    *next;
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
    char                 *name;
#endif
    struct condition     *conditions;
    struct action        *actions;
    struct beta_node     *node;
    struct instantiation *instantiations;
    int                   num_unbound_vars;
    support_type_t        support;
    bool_t                justification;
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
    struct alpha_node   *siblings;
    struct beta_node    *children;
    struct right_memory *right_memories;
};

struct right_memory {
    struct wme *wme;
    struct right_memory *next_in_alpha_node;
};

/*
 * Beta Memory
 */

#define RELATIONAL_TYPE_BITS   1
#define RELATIONAL_TYPE_SHIFT  (FIELD_SHIFT - RELATIONAL_TYPE_BITS)
#define RELATIONAL_TYPE_MASK   ((((unsigned) (~0)) << RELATIONAL_TYPE_SHIFT) & ~FIELD_MASK)

#define ASSERT_VALID_RELATIONAL_TYPE(r) \
    ASSERT(((r) & ~RELATIONAL_TYPE_MASK) == 0, ("bad relational type %d", (r)))

typedef enum relational_type {
    relational_type_constant = 0 << RELATIONAL_TYPE_SHIFT,
    relational_type_variable = 1 << RELATIONAL_TYPE_SHIFT
} relational_type_t;

#define BETA_TEST_TYPE_MASK (~(FIELD_MASK | RELATIONAL_TYPE_MASK))

struct beta_test {
    unsigned bits;
    union {
        unsigned           raw;
        symbol_t           constant_referent;
        variable_binding_t variable_referent;
        struct beta_test  *disjuncts;
    } data;
    struct beta_test *next;
};

#define GET_BETA_TEST_FIELD(t)    GET_FIELD((t)->bits)
#define SET_BETA_TEST_FIELD(t, f) SET_FIELD((t)->bits, (f))

#define GET_BETA_TEST_RELATIONAL_TYPE(t) ((t)->bits & RELATIONAL_TYPE_MASK)
#define SET_BETA_TEST_RELATIONAL_TYPE(t, r) \
    (ASSERT_VALID_RELATIONAL_TYPE(r), (t)->bits &= ~RELATIONAL_TYPE_MASK, (t)->bits |= (r))

#define ASSERT_VALID_TEST_TYPE(ty) \
    ASSERT(((ty) & ~BETA_TEST_TYPE_MASK) == 0, ("bad test type %d", (ty)))

#define GET_BETA_TEST_TYPE(t) ((t)->bits & BETA_TEST_TYPE_MASK)
#define SET_BETA_TEST_TYPE(t, ty) \
    (ASSERT_VALID_TEST_TYPE(ty), (t)->bits &= ~BETA_TEST_TYPE_MASK, (t)->bits |= (ty))

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
    struct beta_node *parent;
    struct beta_node *siblings;
    struct beta_node *children;

    /* Back-pointer to the alpha node that this beta node is
       `attached' to */
    struct alpha_node *alpha_node;

    /* List of beta nodes that share the alpha node */
    struct beta_node *next_with_same_alpha_node;

    /* The tokens that exist at this beta node */
    /* XXX not needed for positive_join; could production nodes store
       this in the instantiation? */
    struct token *tokens;

    /* For negative nodes, the tokens that match the node and
       are ``blocked'' from propagating through. */
    /* XXX only needed by negative */
    struct token *blocked;

    union {
        /* if type in { positive_join, negative }, the tests to apply at this node */
        struct beta_test  *tests;

        /* if type == production, the the production that's been matched */
        struct production *production;
    } data;
};

/*
 * Tokens
 */
struct token {
    /* Back-pointer to the beta node that owns the token. */
    struct beta_node *node;

    /* The next token in the list of tokens owned by the beta node. */
    struct token *next;

    /* The token that this token extends. */
    struct token *parent;

    /* The wme that the token stands for. */
    struct wme *wme;

    /* Set to true if the token is referenced from an old
       instantiation for backtracing purposes. A shared token will not
       be destroyed when removed from the rete network.

       XXX this should be stuffed into the low bit one of the
       pointers, above. */
    bool_t shared;
};


struct match {
    struct production *production;
    union {
        struct token         *token;         /* for assertions */
        struct instantiation *instantiation; /* for retractions */
    } data;
    struct match *next;
};

/* ---------------------------------------------------------------------- */

/*
 * The agent's goal stack.
 */
struct goal_stack {
    symbol_t              symbol;
    struct instantiation *instantiations;
    struct goal_stack    *next;
};

/*
 * An `agent', which is everything that's needed to maintain the state
 * of a Soar process.
 */
struct agent {
    /* The identifier map. */
    unsigned char *id_entries;
    int nid_entries;
    int nfree_entries;

    /* For the RETE network */
    struct beta_node   *root_node;
    struct token        root_token;
    struct alpha_node  *alpha_nodes[16];
    struct goal_stack  *goals;
#ifdef CONF_SOAR_CHUNKING
    int                 bottom_level;
#endif
    struct match       *assertions;
    struct match       *retractions;

    /* For working memory */
    struct ht         slots;
    struct slot_list *modified_slots;
};

/* ---------------------------------------------------------------------- */

/*
 * Initialize the network
 */
extern void
rete_init(struct agent *agent);

#ifdef CONF_SOAR_RETE_CREATE
extern void
rete_create(struct agent *agent);
#endif

extern int
rete_get_instantiation_level(struct agent         *agent,
                             struct instantiation *inst);

struct alpha_node *
rete_find_alpha_node(struct agent *agent,
                     symbol_t      id,
                     symbol_t      attr,
                     symbol_t      value,
                     wme_type_t    type);

struct alpha_node *
rete_ensure_alpha_node(struct agent *agent,
                       symbol_t      id,
                       symbol_t      attr,
                       symbol_t      value,
                       wme_type_t    type);

void
rete_excise(struct agent *agent, struct production *prod);

extern void
rete_finish(struct agent *agent);

#ifdef DEBUG
struct symtab;

extern void
rete_dump(struct agent *agent, struct symtab *symtab);
#endif

/*
 * Add a production to the network
 */
extern void
prod_add_production(struct agent *agent, struct production *p);

/*
 * Notify the network that a new working memory element has been
 * added or removed.
 */

typedef enum wme_operation {
    wme_operation_add,
    wme_operation_remove
} wme_operation_t;

extern void
rete_operate_wme(struct agent *agent, struct wme *wme, wme_operation_t op);

extern void
agent_init(struct agent *agent);

extern void
agent_reset(struct agent *agent);

extern void
agent_finish(struct agent *agent);

extern void
agent_elaborate(struct agent *agent);

extern void
agent_state_no_change(struct agent *agnet, symbol_t superstate);

extern void
agent_operator_no_change(struct agent *agent, symbol_t superstate);

extern void
agent_operator_conflict(struct agent *agent, symbol_t superstate, struct symbol_list *operators);

extern void
agent_operator_tie(struct agent *agent, symbol_t superstate, struct symbol_list *operators);

extern void
agent_pop_subgoals(struct agent *agent, struct goal_stack *bottom);

extern bool_t
agent_is_goal(struct agent *agent, symbol_t id);

extern symbol_t
rete_get_variable_binding(variable_binding_t binding, struct token *token);

extern void
wmem_init(struct agent *agent);

extern void
wmem_finish(struct agent *agent);

extern void
wmem_clear(struct agent *agent);

extern struct preference *
wmem_add_preference(struct agent *agent,
                    symbol_t id, symbol_t attr, symbol_t value,
                    preference_type_t type,
                    support_type_t support);

extern void
wmem_remove_preference(struct agent *agent, struct preference *pref, bool_t save);

extern struct preference *
wmem_get_preferences(struct agent *agent, symbol_t id, symbol_t attr);

extern struct wme *
wmem_get_wmes(struct agent *agent, symbol_t id, symbol_t attr);

extern void
wmem_elaborate(struct agent *agent);

typedef void (*wme_enumerator_t)(struct agent *agent, struct wme *wme, void *closure);

extern void
wmem_enumerate_wmes(struct agent *agent, wme_enumerator_t enumerator, void *closure);

extern void
wmem_remove_instantiation(struct agent          *agent,
                          struct instantiation  *inst,
                          struct token         **queue);

struct wmem_sweep_data {
    struct agent *agent;
    int           level;
};

extern ht_enumerator_result_t
wmem_sweep_subgoals(struct ht_entry_header *header, void *closure);

extern void
agent_reserve_identifiers(struct agent *agent, int count);

extern symbol_t
agent_get_identifier(struct agent *agent);

extern int
agent_get_id_level(struct agent *agent, symbol_t id);

extern void
agent_set_id_level(struct agent *agent, symbol_t id, int level);

#ifdef CONF_SOAR_CHUNKING
extern void
chunk_if_results(struct agent         *agent,
                 struct instantiation *inst,
                 struct preference    *o_rejects,
                 int                   level);
#endif

#ifdef DEBUG
extern struct symtab symtab;

void
debug_indent_by(int nest);

const char *
debug_test_type_to_string(test_type_t type);

const char *
debug_symbol_to_string(struct symtab *symtab, symbol_t symbol);

void
debug_dump_wme(struct symtab *symtab, struct wme *wme);

void
debug_dump_token(struct symtab *symtab, struct token *token);

void
debug_dump_variable_binding(variable_binding_t binding);

void
debug_dump_test(struct symtab *symtab, struct beta_test *test);

const char *
debug_preference_type_to_string(preference_type_t type);

void
debug_dump_rhs_value(struct symtab *symtab, struct rhs_value *value);

void
debug_dump_beta_node(struct symtab    *symtab,
                     struct beta_node *node,
                     int               nest,
                     int               recur,
                     int               tokens);

void
debug_dump_alpha_node(struct symtab *symtab, struct alpha_node *alpha, bool_t acceptable);

void
debug_dump_preference(struct symtab *symtab, struct preference *pref);
#endif /* DEBUG */

#endif /* soar_h__ */
