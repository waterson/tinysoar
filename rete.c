/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*

  The Rete Network


  TO DO
  -----

  . Debug simple negative conditions

  . Implement o-support

  . Implement conjunctive negative conditions

  . Factory redundancy between create_*_node() routines.

*/

#include "soar.h"
#include "alloc.h"

static void
do_left_addition(struct agent* agent,
                 struct beta_node* node,
                 struct token* token,
                 struct wme* wme);

static void
do_left_removal(struct agent* agent,
                struct beta_node* node,
                struct token* token,
                struct wme* wme);

static void
do_right_addition(struct agent* agent,
                  struct beta_node* node,
                  struct wme* wme);

static void
do_right_removal(struct agent* agent,
                 struct beta_node* node,
                 struct wme* wme);


struct variable_binding_list {
    symbol_t                      variable;
    variable_binding_t            binding;
    struct variable_binding_list* next;
};


/*
 * Determine if two beta_test lists are identical. Kinda lame,
 * because it doesn't handle goofy ordering, but oh well.
 */
static bool_t
beta_tests_are_identical(struct beta_test* left, struct beta_test* right)
{
    for ( ; left && right; left = left->next, right = right->next) {
        if (left->type != right->type)
            return 0;

        switch (left->type) {
        case test_type_conjunctive:
            /* shouldn't hit this; conjunctive tests are converted
               into a list of single tests. */
            ERROR(("unexpected conjunctive test"));
            break;

        case test_type_disjunctive:
            if (! beta_tests_are_identical(left->data.disjuncts,
                                           right->data.disjuncts))
                return 0;

            break;

        case test_type_blank:
        case test_type_goal_id:
        case test_type_impasse_id:
            /* Yep, these are ok. */
            break;

        case test_type_equality:
        case test_type_not_equal:
        case test_type_less:
        case test_type_greater:
        case test_type_less_or_equal:
        case test_type_greater_or_equal:
        case test_type_same_type:
            /* Are the tests for the same field? */
            if (left->field != right->field)
                return 0;

            /* Do they test the same kind of relation? */
            if (left->relational_type != right->relational_type)
                return 0;

            /* Are the referents the same? */
            if (left->relational_type == relational_type_constant) {
                if (! SYMBOLS_ARE_EQUAL(left->data.constant_referent,
                                        right->data.constant_referent))
                    return 0;
            }
            else {
                if (! VARIABLE_BINDINGS_ARE_EQUAL(left->data.variable_referent,
                                                  right->data.variable_referent))
                    return 0;
            }
        }
    }

    return left == right;
}

/*
 * Free a list of beta tests
 */
static void
free_beta_tests(struct agent* agent, struct beta_test* tests)
{
    while (tests) {
        struct beta_test* doomed = tests;
        tests = tests->next;
        free(doomed);
    }
}

/*
 * Create a new token
 */
static inline struct token*
create_token(struct beta_node* node,
             struct token* parent,
             struct wme* wme)
{
    struct token* result = (struct token*) malloc(sizeof(struct token));
    result->parent = parent;
    result->node = node;
    result->wme = wme;
    return result;
}

/*
 * Are the two tokens equal?
 */
static bool_t
tokens_are_equal(struct token* left, struct token* right)
{
    while (left && right) {
        /* If we ever get to a point where left and right refer to the
           same token, then the tokens are most certainly equal */
        if (left == right)
            return 1;

        /* If the wmes don't match, the tokens are different */
        if (left->wme != right->wme)
            return 0;

        /* Walk up to the parent */
        left = left->parent;
        right = right->parent;
    }

    return (left == 0) && (right == 0);
}

/*
 * Given an id, attr, value, and alpha-type, determine which alpha
 * memory bucket a test should be in.
 */
static inline short
get_alpha_test_index(symbol_t id,
                     symbol_t attr,
                     symbol_t value,
                     wme_type_t type)
{
    return ((type == wme_type_acceptable) ? 8 : 0) |
        (GET_SYMBOL_VALUE(id) ? 4 : 0) |
        (GET_SYMBOL_VALUE(attr) ? 2 : 0) |
        (GET_SYMBOL_VALUE(value) ? 1 : 0);
}

/*
 * Select a field from the specified wme.
 */
static symbol_t
get_field_from_wme(struct wme* wme, field_t field)
{
    ASSERT(wme != 0, ("null ptr"));

    switch (field) {
    case field_id:
        return wme->slot->id;

    case field_attr:
        return wme->slot->attr;

    default:
        /* XXX gcc generates better code when we write the switch
           statement this way. */
        ASSERT(field == field_value, ("unexpected value for `field_t'"));
        break;
    }

    return wme->value;
}

/*
 * Determine if a working memory element matches an alpha node
 */
static inline bool_t
wme_matches_alpha_node(const struct wme* wme, const struct alpha_node* node)
{
    return (SYMBOL_IS_NIL(node->id) ||
            SYMBOLS_ARE_EQUAL(node->id, wme->slot->id)) &&
        (SYMBOL_IS_NIL(node->attr) ||
         SYMBOLS_ARE_EQUAL(node->attr, wme->slot->attr)) &&
        (SYMBOL_IS_NIL(node->value) ||
         SYMBOLS_ARE_EQUAL(node->value, wme->value));
}

/*
 * Add a wme to the specified alpha node's right memory
 */
static void
add_wme_to_alpha_node(struct agent* agent,
                      struct alpha_node* node,
                      struct wme* wme)
{
    struct right_memory* rm =
        (struct right_memory*) malloc(sizeof(struct right_memory));

    rm->wme = wme;
    rm->next_in_alpha_node = node->right_memories;
    node->right_memories = rm;
}

/*
 * Remove a wme from the specified alpha node's right memory
 */
static void
remove_wme_from_alpha_node(struct agent* agent,
                           struct alpha_node* node,
                           struct wme* wme)
{
    struct right_memory* rm = node->right_memories;
    struct right_memory** link = &node->right_memories;

    while (rm) {
        if (rm->wme == wme) {
            *link = rm->next_in_alpha_node;
            free(rm);
            break;
        }

        link = &rm->next_in_alpha_node;
        rm = rm->next_in_alpha_node;
    }
}

/*
 * Find an existing alpha node that is appropriate for testing the
 * specified fields.
 *
 * Corresponds to find_alpha_mem() in rete.c from Soar8.
 */
static struct alpha_node*
find_alpha_node(struct agent* agent,
                symbol_t id,
                symbol_t attr,
                symbol_t value,
                wme_type_t type)
{
    struct alpha_node* node =
        agent->alpha_nodes[get_alpha_test_index(id, attr, value, type)];

    for ( ; node != 0; node = node->siblings) {
        if (SYMBOLS_ARE_EQUAL(id, node->id) &&
            SYMBOLS_ARE_EQUAL(attr, node->attr) &&
            SYMBOLS_ARE_EQUAL(value, node->value))
            return node;
    }
    return 0;
}

static void
add_matching_wmes(struct agent* agent, struct wme* wme, void* closure)
{
    struct alpha_node* alpha_node = (struct alpha_node*) closure;
    if (wme_matches_alpha_node(wme, alpha_node))
        add_wme_to_alpha_node(agent, alpha_node, wme);
}

/*
 * Find an existing alpha node that is appropriate for testing the
 * specified fields. If none exists, create a new one.
 *
 * This corresponds to find_or_make_alpha_mem() in rete.c from Soar8.
 */
static struct alpha_node*
ensure_alpha_node(struct agent* agent,
                  symbol_t id,
                  symbol_t attr,
                  symbol_t value,
                  wme_type_t type)
{
    struct alpha_node* result;

    if (! (result = find_alpha_node(agent, id, attr, value, type))) {
        struct alpha_node** head =
            &agent->alpha_nodes[get_alpha_test_index(id, attr, value, type)];

        struct alpha_node* more_general_node;
        symbol_t nil;

        result = (struct alpha_node*) malloc(sizeof(struct alpha_node));
        result->id    = id;
        result->attr  = attr;
        result->value = value;
        result->siblings = *head;
        result->children = 0;
        result->right_memories = 0;
        *head = result;

        /* Fill in the new memory with any matching wmes */
        more_general_node = 0;
        CLEAR_SYMBOL(nil);

        if (! SYMBOL_IS_NIL(id))
            more_general_node = find_alpha_node(agent, nil, attr, value, type);

        if (! more_general_node && ! SYMBOL_IS_NIL(value))
            more_general_node = find_alpha_node(agent, nil, attr, nil, type);

        if (more_general_node) {
            /* Found a more general working memory; use it to fill in
               our right memory */
            struct right_memory* rm;
            for (rm = more_general_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (wme_matches_alpha_node(rm->wme, result))
                    add_wme_to_alpha_node(agent, result, rm->wme);
            }
        }
        else {
            /* Troll through *all* the wmes */
            wmem_enumerate_wmes(agent, add_matching_wmes, result);
        }
    }

    return result;
}

/*
 * Look through a variable binding list to find the binding for the
 * specified variable.
 */
static inline const variable_binding_t*
find_bound_variable(const struct variable_binding_list* bindings,
                    const symbol_t variable)
{
    for ( ; bindings != 0; bindings = bindings->next) {
        if (SYMBOLS_ARE_EQUAL(bindings->variable, variable))
            return &bindings->binding;
    }

    return 0;
}

/*
 * Compute a `variable binding' for each variable contained in the
 * specified test. If the variable binding list already contains a
 * binding for the specified variable, then we'll leave it
 * unchanged. If there is no binding for the specified variable in the
 * binding list, we'll make one.
 *
 * Note that the computed depth of the variable binding will be
 * `absolute'. This will need to be made into a `relative depth'
 * before being stored in the beta_test struct.
 */
static void
bind_variables(const struct test* test,
               unsigned depth,
               field_t field,
               struct variable_binding_list** bindings)
{
    switch (test->type) {
    case test_type_equality:
    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
        if ((GET_SYMBOL_TYPE(test->data.referent) == symbol_type_variable) &&
            !find_bound_variable(*bindings, test->data.referent)) {
            struct variable_binding_list* entry =
                (struct variable_binding_list*)
                malloc(sizeof(struct variable_binding_list));

            entry->variable = test->data.referent;
            entry->binding.depth = depth;
            entry->binding.field = field;
            entry->next = *bindings;
            *bindings = entry;
        }
        break;

    case test_type_conjunctive:
        {
            struct test_list* tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next)
                bind_variables(&tests->test, depth, field, bindings);
        }
        break;

    case test_type_disjunctive:
        UNIMPLEMENTED(); /* XXX write me! */
        break;

    case test_type_goal_id:
    case test_type_impasse_id:
    case test_type_blank:
        /* XXX do nothing? */
        break;
    }
}

/*
 * Convert a `test' from a condition into the appropriate RETE
 * structures: constants that are tested by the alpha network and
 * nodes in the beta network.
 *
 * This corresponds to add_rete_tests_for_test() from rete.c in Soar8.
 */
static void
process_test(const struct test* test,
             unsigned depth,
             field_t field,
             const struct variable_binding_list* bindings,
             symbol_t* constant,
             struct beta_test** beta_tests)
{
    struct beta_test* beta_test = 0;
    symbol_type_t symbol_type;

    if (test->type == test_type_blank)
        return;

    symbol_type = GET_SYMBOL_TYPE(test->data.referent);

    switch (test->type) {
    case test_type_equality:
        if (symbol_type != symbol_type_variable && !constant->val) {
            /* It's a constant, and we can install an alpha test */
            *constant = test->data.referent;
            break;
        }

        /* otherwise, fall through */

    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
        if (symbol_type == symbol_type_variable) {
            /* It's a variable. Make a variable relational test */
            const variable_binding_t* binding;
            beta_test = (struct beta_test*) malloc(sizeof(struct beta_test));

            binding = find_bound_variable(bindings, test->data.referent);
            ASSERT(binding != 0, ("null ptr"));

            beta_test->relational_type = relational_type_variable;
            beta_test->data.variable_referent = *binding;

            /* Fix up the variable referent's depth (which was stored
               as an `absolute depth' in the binding list) to be
               relative to the current depth of the test. */
            beta_test->data.variable_referent.depth =
                depth - beta_test->data.variable_referent.depth;
        }
        else {
            beta_test =
                (struct beta_test*) malloc(sizeof(struct beta_test));

            beta_test->relational_type = relational_type_constant;
            beta_test->data.constant_referent = test->data.referent;
        }
        break;

    case test_type_goal_id:
    case test_type_impasse_id:
        beta_test = (struct beta_test*) malloc(sizeof(struct beta_test));
        break;

    case test_type_blank:
        /* do nothing */
        break;

    case test_type_conjunctive:
        {
            struct test_list* tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next)
                process_test(&tests->test, depth, field, bindings, constant, beta_tests);
        }
        break;

    case test_type_disjunctive:
        UNIMPLEMENTED(); /* XXX just hit a test that needs writin' */

    default:
        ERROR(("shouldn't get here"));
    }

    if (beta_test) {
        beta_test->type  = test->type;
        beta_test->field = field;
        beta_test->next  = *beta_tests;
        *beta_tests      = beta_test;
    }
}

/*
 * Check a single beta test
 */
static bool_t
check_beta_test(struct agent* agent,
                struct beta_test* test,
                struct token* token,
                struct wme* wme)
{
    switch (test->type) {
    case test_type_equality:
    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
        {
            /* It's a relational test */
            symbol_t right = get_field_from_wme(wme, test->field);
            symbol_t left;

            if (test->relational_type == relational_type_constant) {
                left = test->data.constant_referent;
            }
            else {
                struct wme* left_wme = wme;
                int depth = (int) test->data.variable_referent.depth;
                if (depth) {
                    struct token* t = token;
                    while (--depth > 0)
                        t = t->parent;
                    left_wme = t->wme;
                }

                left = get_field_from_wme(left_wme, test->data.variable_referent.field);
            }

            switch (test->type) {
            case test_type_equality:
                if (SYMBOLS_ARE_EQUAL(left, right))
                    return 1;

                break;

            case test_type_not_equal:
                if (! SYMBOLS_ARE_EQUAL(left, right))
                    return 1;

                break;

            case test_type_less:
            case test_type_greater:
            case test_type_less_or_equal:
            case test_type_greater_or_equal:
            case test_type_same_type:
                /* If the types differ, any of these tests would fail */
                if (GET_SYMBOL_TYPE(left) != GET_SYMBOL_TYPE(right))
                    return 0;

                /* We're done if all we were testing was `same type' */
                if (test->type == test_type_same_type)
                    return 1;

                /* If we're doing relational tests, then they'll
                   only work on integer constants */
                if (GET_SYMBOL_TYPE(left) == symbol_type_integer_constant) {
                    int result = GET_SYMBOL_VALUE(right) - GET_SYMBOL_VALUE(left);
                    switch (test->type) {
                    case test_type_less:
                        if (result < 0) return 1;
                        break;

                    case test_type_greater:
                        if (result > 0) return 1;
                        break;

                    case test_type_less_or_equal:
                        if (result <= 0) return 1;
                        break;

                    case test_type_greater_or_equal:
                        if (result >= 0) return 1;
                        break;

                    default:
                        UNREACHABLE(); /* never reached */
                    }
                }

                break;

            default:
                UNREACHABLE(); /* never reached */
            }
        }
        break;

    case test_type_goal_id:
        {
            struct symbol_list* goal;
            for (goal = agent->goals; goal != 0; goal = goal->next) {
                if (SYMBOLS_ARE_EQUAL(goal->symbol, wme->slot->id))
                    return 1;
            }
        }
        break;

    case test_type_impasse_id:
        {
            struct symbol_list* impasse;
            for (impasse = agent->impasses; impasse != 0; impasse = impasse->next) {
                if (SYMBOLS_ARE_EQUAL(impasse->symbol, wme->slot->id))
                    return 1;
            }
        }
        break;

    case test_type_disjunctive:
        {
            struct beta_test* disjunct;
            for (disjunct = test->data.disjuncts; disjunct != 0; disjunct = disjunct->next) {
                if (check_beta_test(agent, disjunct, token, wme))
                    return 1;
            }
        }
        break;

    case test_type_conjunctive:
    case test_type_blank:
        /* shouldn't ever hit this; conjunctive tests are
           converted into a list of single tests. */
        ERROR(("unexpected test"));
        break;
    }

    /* If we get here, the test failed */
    return 0;
}

/*
 * Check a list of beta tests
 */
static inline bool_t
check_beta_tests(struct agent* agent,
                 struct beta_test* test,
                 struct token* token,
                 struct wme* wme)
{
    for ( ; test != 0; test = test->next) {
        if (! check_beta_test(agent, test, token, wme))
            return 0;
    }

    /* If we get here, all the tests passed. */
    return 1;
}

/*
 * Propagate matches from the parent node to its child.
 *
 * This corresponds to update_node_with_matches_from_above() from
 * rete.c in Soar8.
 */
static void
initialize_matches(struct agent* agent,
                   struct beta_node* child,
                   struct beta_node* parent)
{
    if (parent->type == beta_node_type_root) {
        do_left_addition(agent, child, &agent->root_token, 0);
    }
    else if (parent->type & beta_node_type_bit_positive) {
        /* Temporarily splice out all of parent's children except
           `child', then call the right-addition routine, and restore
           parent's children. */
        struct beta_node* old_children = parent->children;
        struct beta_node* old_siblings = child->siblings;
        struct right_memory* rm;

        parent->children = child;
        child->siblings = 0;

        for (rm = parent->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node)
            do_right_addition(agent, parent, rm->wme);

        parent->children = old_children;
        child->siblings = old_siblings;
    }
    else if (parent->type & beta_node_type_bit_negative) {
        struct token* token;
        for (token = parent->tokens; token != 0; token = token->next)
            do_left_addition(agent, parent, token, 0); /* XXX not right */
    }
    else {
        UNIMPLEMENTED(); /* XXX write me! */
    }
}


/*
 * Create a beta memory node with the specified parent.
 */
static struct beta_node*
create_memory_node(struct agent* agent, struct beta_node* parent)
{
    struct beta_node* result;

    result = (struct beta_node*) malloc(sizeof(struct beta_node));
    result->type       = beta_node_type_memory;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;
    result->tokens     = 0;

    initialize_matches(agent, result, parent);

    return result;
}

/*
 * Create a positive join node with the specified parent.
 */
static struct beta_node*
create_positive_join_node(struct agent* agent,
                          struct beta_node* parent,
                          struct alpha_node* alpha_node,
                          struct beta_test* tests)
{
    struct beta_node* result =
        (struct beta_node*) malloc(sizeof(struct beta_node));

    result->type       = beta_node_type_positive_join;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;

    result->alpha_node = alpha_node;
    result->next_with_same_alpha_node = alpha_node->children;
    alpha_node->children = result;

    result->tokens = 0;

    result->data.tests = tests;

    return result;
}


static struct beta_node*
create_negative_node(struct agent* agent,
                     struct beta_node* parent,
                     struct alpha_node* alpha_node,
                     struct beta_test* tests)
{
    struct beta_node* result =
        (struct beta_node*) malloc(sizeof(struct beta_node));

    result->type       = beta_node_type_negative;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;

    result->alpha_node = alpha_node;
    result->next_with_same_alpha_node = alpha_node->children;
    alpha_node->children = result;

    result->tokens     = 0;
    result->blocked    = 0;

    result->data.tests = tests;

    initialize_matches(agent, result, parent);

    return result;
}

/*
 * Create a production node with the specified parent
 */
static struct beta_node*
create_production_node(struct agent* agent,
                       struct beta_node* parent,
                       struct production* production)
{
    struct beta_node* result;

    result = (struct beta_node*) malloc(sizeof(struct beta_node));
    result->type       = beta_node_type_production;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;
    result->tokens     = 0;
    result->data.production = production;

    /* XXX why not? */
    /* initialize_matches(agent, result, parent); */

    return result;
}


/*
 * Convert variable symbols into the corresponding variable binding.
 */
static void
process_rhs_value(struct rhs_value* value,
                  struct variable_binding_list* bindings,
                  unsigned depth)
{
    if ((value->type == rhs_value_type_symbol) &&
        (value->val.symbol.type == symbol_type_variable)) {
        /* Look up the binding for the variable. There'd better be
           one, or this trip'll end real quick. */
        const variable_binding_t* binding =
            find_bound_variable(bindings, value->val.symbol);

        ASSERT(binding != 0, ("null ptr"));

        /* Replace the symbol with an the variable binding */
        value->type = rhs_value_type_variable_binding;
        value->val.variable_binding = *binding;

        /* Fix the depth (which was stored as an `absolute depth' in
           the binding list) to be relative to the current depth of
           the node in the rete network. */
        value->val.variable_binding.depth =
            depth - value->val.variable_binding.depth;
    }
}

/*
 * Find or create a beta node for the given single condition `cond',
 * which must be a simple positive condition. The new node is a child
 * of `parent'.
 *
 * This corresponds to make_node_for_positive_cond() in rete.c in
 * Soar8.
 */
static struct beta_node*
ensure_positive_condition_node(struct agent* agent,
                               const struct condition* cond,
                               unsigned depth,
                               struct beta_node* parent,
                               struct variable_binding_list** bindings)
{
    struct beta_node* result;
    struct beta_node* memory_node;
    struct beta_test* tests = 0;
    symbol_t alpha_id, alpha_attr, alpha_value;
    struct alpha_node* alpha_node;

    CLEAR_SYMBOL(alpha_id);
    CLEAR_SYMBOL(alpha_attr);
    CLEAR_SYMBOL(alpha_value);

    /* XXX gee, that's a lot of parameters. */
    bind_variables(&cond->data.simple.id_test,    depth, field_id,    bindings);
    bind_variables(&cond->data.simple.attr_test,  depth, field_attr,  bindings);
    bind_variables(&cond->data.simple.value_test, depth, field_value, bindings);

    process_test(&cond->data.simple.id_test,    depth, field_id,    *bindings, &alpha_id,    &tests);
    process_test(&cond->data.simple.attr_test,  depth, field_attr,  *bindings, &alpha_attr,  &tests);
    process_test(&cond->data.simple.value_test, depth, field_value, *bindings, &alpha_value, &tests);

    /* See if there's a memory node we can use */
    for (memory_node = parent->children; memory_node != 0; memory_node = memory_node->siblings) {
        if (memory_node->type == beta_node_type_memory)
            break;
    }

    if (memory_node) {
        for (result = memory_node->children; result != 0; result = result->siblings) {
            if ((result->type == beta_node_type_positive_join)
                && (SYMBOLS_ARE_EQUAL(result->alpha_node->id, alpha_id))
                && (SYMBOLS_ARE_EQUAL(result->alpha_node->attr, alpha_attr))
                && (SYMBOLS_ARE_EQUAL(result->alpha_node->value, alpha_value))
                && beta_tests_are_identical(result->data.tests, tests))
                break;
        }

        if (! result) {
            /* If we didn't a matching positive-join node, make one
               now, parented by the memory node. */
            alpha_node =
                ensure_alpha_node(agent, alpha_id, alpha_attr, alpha_value,
                                  cond->acceptable);

            return create_positive_join_node(agent, memory_node, alpha_node, tests);
        }

        /* Hey, we found one. Free the tests we allocated and return
           the node we found. */
        free_beta_tests(agent, tests);
        return result;
    }

    /* XXX At this point, if we'd implemented `memory/positive-join'
       nodes, we'd want to grovel through parent's children to see if
       there was an MP node that we could split to produce a single
       memory node with two positive-join children. */

    /* No memory node, so create one and attach a positive join node to it. */
    memory_node = create_memory_node(agent, parent);

    alpha_node =
        ensure_alpha_node(agent, alpha_id, alpha_attr, alpha_value, cond->acceptable);

    return create_positive_join_node(agent, memory_node, alpha_node, tests);
}



/*
 * Find or create a beta node for the given single condition `cond',
 * which must be a simple ncondition. The new node is a child
 * of `parent'.
 *
 * This corresponds to make_node_for_negative_cond() in rete.c in
 * Soar8.
 */
static struct beta_node*
ensure_negative_condition_node(struct agent* agent,
                               const struct condition* cond,
                               unsigned depth,
                               struct beta_node* parent,
                               struct variable_binding_list** bindings)
{
    struct beta_node* result;
    struct beta_test* tests = 0;
    symbol_t alpha_id, alpha_attr, alpha_value;
    struct alpha_node* alpha_node;

    CLEAR_SYMBOL(alpha_id);
    CLEAR_SYMBOL(alpha_attr);
    CLEAR_SYMBOL(alpha_value);

    /* XXX gee, that's a lot of parameters. */
    bind_variables(&cond->data.simple.id_test,    depth, field_id,    bindings);
    bind_variables(&cond->data.simple.attr_test,  depth, field_attr,  bindings);
    bind_variables(&cond->data.simple.value_test, depth, field_value, bindings);

    process_test(&cond->data.simple.id_test,    depth, field_id,    *bindings, &alpha_id,    &tests);
    process_test(&cond->data.simple.attr_test,  depth, field_attr,  *bindings, &alpha_attr,  &tests);
    process_test(&cond->data.simple.value_test, depth, field_value, *bindings, &alpha_value, &tests);

    /* See if there's already a negative node we can use */
    for (result = parent->children; result != 0; result = result->siblings) {
        if ((result->type == beta_node_type_negative)
            && (SYMBOLS_ARE_EQUAL(result->alpha_node->id, alpha_id))
            && (SYMBOLS_ARE_EQUAL(result->alpha_node->attr, alpha_attr))
            && (SYMBOLS_ARE_EQUAL(result->alpha_node->value, alpha_value))
            && beta_tests_are_identical(result->data.tests, tests))
            break;
    }

    if (! result) {
        /* If we didn't a matching negative node, make one now. */
        alpha_node =
            ensure_alpha_node(agent, alpha_id, alpha_attr, alpha_value,
                              cond->acceptable);

        return create_negative_node(agent, parent, alpha_node, tests);
    }

    /* Hey, we found one. Free the tests we allocated and return
       the node we found. */
    free_beta_tests(agent, tests);
    return result;
}


/* ---------------------------------------------------------------------- */

/*
 * Notify the beta node |node| that a new token |token| is being
 * propagated downward from its parent beta node. The working memory
 * element |wme| may extend |token| if the parent node has no storage
 * of its own.
 */
static void
do_left_addition(struct agent* agent,
                 struct beta_node* node,
                 struct token* token,
                 struct wme* wme)
{
    switch (node->type) {
    case beta_node_type_memory:
        {
            /* Add a new token to the memory and notify children */
            struct token* new_token;
            struct beta_node* child;

            new_token = create_token(node, token, wme);
            new_token->next = node->tokens;
            node->tokens = new_token;

            for (child = node->children; child != 0; child = child->siblings)
                do_left_addition(agent, child, new_token, 0);
        }
        break;

    case beta_node_type_positive_join:
        {
            struct right_memory* rm;
            for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                    struct beta_node* child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_addition(agent, child, token, rm->wme);
                }
            }
        }
        break;

    case beta_node_type_negative:
        {
            struct token* new_token;
            struct right_memory* rm;

            ASSERT(wme != 0, ("no wme in left-addition to negative node"));
            new_token = create_token(node, token, wme);

            /* Iterate through the right-memories to see if the token
               is blocked. */
            for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (check_beta_tests(agent, node->data.tests, new_token, rm->wme)) {
                    new_token->next = node->blocked;
                    node->blocked = new_token;
                    break;
                }
            }

            if (! rm) {
                /* Nothing matches the negative node, so go ahead and
                   propagate the token downwards. */
                struct beta_node* child;

                new_token->next = node->tokens;
                node->tokens = new_token;

                for (child = node->children; child != 0; child = child->siblings)
                    do_left_addition(agent, child, new_token, 0);
            }
        }
        break;

    case beta_node_type_production:
        {
            struct token* new_token = create_token(node, token, wme);
            struct match* match;

            new_token->next = node->tokens;
            node->tokens = new_token;

            /* XXX Soar8 checks the retraction queue to see if the
               match has been retracted, and if so, removes the match
               from the retraction queue. We've got a simpler ownership
               model for tokens, so it's not possible to do that. Is
               this gonna be a problem? */

            /* Allocate a new match and place on the firing queue */
            match = (struct match*) malloc(sizeof(struct match));
            match->data.token = new_token;
            match->production = node->data.production;
            match->next = agent->assertions;
            agent->assertions = match;
        }
        break;

    case beta_node_type_memory_positive_join:
    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
        UNIMPLEMENTED(); /* XXX write me! */
        break;

    case beta_node_type_root:
        ERROR(("unexpected left addition")); /* can't get left addition on this node */
        break;
    }
}


static void
do_left_removal(struct agent* agent,
                struct beta_node* node,
                struct token* token,
                struct wme* wme)
{
    switch (node->type) {
    case beta_node_type_memory:
        {
            struct token* doomed;
            struct token **link;

            for (doomed = node->tokens, link = &node->tokens;
                 doomed != 0;
                 link = &doomed->next, doomed = doomed->next) {
                if (doomed->wme == wme) {
                    struct beta_node* child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_removal(agent, child, doomed, 0);

                    *link = doomed->next;
                    free(doomed);
                    break;
                }
            }
        }
        break;

    case beta_node_type_positive_join:
        {
            struct right_memory* rm;
            for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                    struct beta_node* child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_removal(agent, child, token, rm->wme);
                }
            }
        }
        break;

    case beta_node_type_negative:
        {
            struct token** link;
            struct token* doomed;

            /* First see if this was a ``blocked'' token, in which
               case its removal will have no side effects */
            for (link = &node->blocked, doomed = *link;
                 doomed != 0;
                 link = &doomed->next, doomed = *link) {
                if (doomed->wme == wme) {
                    *link = doomed->next;
                    free(doomed);
                    break;
                }
            }

            if (! doomed) {
                /* Okay, it must've been one of the tokens that we'd propagated
                   downwards. Find it and yank it. */
                for (link = &node->tokens, doomed = *link;
                     doomed != 0;
                     link = &doomed->next, doomed = *link) {
                    if (doomed->wme == wme) {
                        struct beta_node* child;
                        for (child = node->children; child != 0; child = child->siblings)
                            do_left_removal(agent, child, doomed, 0);

                        *link = doomed->next;
                        free(doomed);
                        break;
                    }
                }
            }

            ASSERT(doomed, ("couldn't find wme in left removal"));
        }
        break;

    case beta_node_type_production:
        {
            struct match* match;

            /* See if this match is new */
            {
                struct match** link;

                for (link = &agent->assertions, match = *link;
                     match != 0;
                     link = &match->next, match = *link) {
                    if ((match->data.token->wme == wme) &&
                        tokens_are_equal(match->data.token->parent, token)) {
                        /* Yep. Remove from the assertion queue */
                        *link = match->next;
                        free(match);
                        break;
                    }
                }
            }

            if (! match) {
                /* It's not a new match. Find the instantiation that
                   we need to retract */
                struct instantiation* inst;
                for (inst = node->data.production->instantiations;
                     inst != 0;
                     inst = inst->next) {
                    if (inst->token->wme == wme /* XXX sufficient? */) {
                        /* See if this match is already on the retraction queue */
                        for (match = agent->retractions; match != 0; match = match->next) {
                            if (match->data.instantiation == inst)
                                break;
                        }

                        if (! match) {
                            /* Gotcha. Allocate a new match and place on the
                               retraction queue */
                            match = (struct match*) malloc(sizeof(struct match));
                            match->data.instantiation = inst;
                            match->production = node->data.production;
                            match->next = agent->retractions;
                            agent->retractions = match;
                        }
                    }
                }
            }

            /* Nuke the token */
            {
                struct token** link;
                struct token* doomed;
                for (link = &node->tokens, doomed = *link;
                     token != 0;
                     link = &doomed->next, doomed = *link) {
                    if (doomed->wme == wme) {
                        *link = doomed->next;
                        free(doomed);
                        break;
                    }
                }

                ASSERT(token, ("couldn't find token in left-removal"));
            }
        }
        break;

    case beta_node_type_memory_positive_join:
    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
        UNIMPLEMENTED(); /* XXX write me! */
        break;

    case beta_node_type_root:
        ERROR(("unexpected left addition")); /* can't get left addition on this node */
        break;
    }
}

/*
 * Notify the beta node |node| that a new wme |wme| has been added to
 * the right-memory to which |node| is attached.
 */
static void
do_right_addition(struct agent* agent, struct beta_node* node, struct wme* wme)
{
    struct token** link;
    struct token* token = node->tokens;

    switch (node->type) {
    case beta_node_type_positive_join:
        /* Iterate through our parent node's tokens, because we won't
           have any of our own. Fall through, after resetting the
           token list */
        ASSERT(node->parent->type == beta_node_type_memory,
               ("unexpected parent node in right addition"));

        token = node->parent->tokens;

        /* fall through... */

    case beta_node_type_memory_positive_join:
        /* Test each token against the WME that's being added. Any
           matches will result in recursive left-additions to our
           children. */
        while (token) {
            if (check_beta_tests(agent, node->data.tests, token, wme)) {
                struct beta_node* child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_addition(agent, child, token, wme);
            }

            token = token->next;
        }
        break;

    case beta_node_type_negative:
        /* Iterate through the ``active'' tokens to see if any will be
           blocked by this addition */
        for (link = &node->tokens, token = *link;
             token != 0;
             link = &token->next, token = *link) {
            if (!node->data.tests || check_beta_tests(agent, node->data.tests, token, wme)) {
                /* If there are no beta tests, or the beta tests
                   all pass, then the negative condition has
                   matched. We need to remove any tokens that had
                   previously been propagated. */
                struct beta_node* child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_removal(agent, child, token, 0);

                /* This token is now ``blocked'' */
                *link = token->next;

                token->next = node->blocked;
                node->blocked = token;
            }
        }
        break;

    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
    case beta_node_type_root:
    case beta_node_type_production:
    case beta_node_type_memory:
        ERROR(("unexpected right addition")); /* can't get right addition on these nodes */
        break;
    }
}

static void
do_right_removal(struct agent* agent, struct beta_node* node, struct wme* wme)
{
    struct token* token = node->tokens;
    struct token** link;

    switch (node->type) {
    case beta_node_type_positive_join:
        /* Iterate through our parent node's tokens, because we won't
           have any of our own. Fall through, after resetting the
           token list */
        ASSERT(node->parent->type == beta_node_type_memory,
               ("unexpected parent node"));

        token = node->parent->tokens;

        /* fall through... */

    case beta_node_type_memory_positive_join:
        /* Test each token against the WME that's being removed. Any
           matches will result in recursive left-removals from our
           children. */
        while (token) {
            if (check_beta_tests(agent, node->data.tests, token, wme)) {
                struct beta_node* child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_removal(agent, child, token, wme);
            }

            token = token->next;
        }
        break;

    case beta_node_type_negative:
        /* Iterate through the blocked tokens to see if any will be
           unblocked by the right-memory removal. */
        for (link = &node->blocked, token = *link;
             token != 0;
             link = &token->next, token = *link) {
            if (!node->data.tests || check_beta_tests(agent, node->data.tests, token, wme)) {
                /* If there are no beta tests, or the beta tests
                   all pass, then this blocked token just became
                   unblocked. Propagate it downward. */
                struct beta_node* child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_addition(agent, child, token, 0);

                /* Move the token to the ``unblocked'' list */
                *link = token->next;

                token->next = node->tokens;
                node->tokens = token;
            }
        }
        break;

    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
    case beta_node_type_root:
    case beta_node_type_production:
    case beta_node_type_memory:
        /* We won't ever get right addition on these nodes, because
           they can't be directly connected ot a right memory. */
        ERROR(("unexpected right addition"));
        break;
    }
}


/* ----------------------------------------------------------------------
 *
 * RETE API Routines
 *
 * ---------------------------------------------------------------------- */

/*
 * Initialize the rete network.
 */
void
rete_init(struct agent* agent)
{
    int i;

    agent->root_node.type = beta_node_type_root;
    agent->root_node.tokens = &agent->root_token;

    agent->root_token.parent = 0;
    agent->root_token.node   = &agent->root_node;
    agent->root_token.wme    = 0;
    agent->root_token.next   = 0;

    for (i = 0; i < (sizeof(agent->alpha_nodes) / sizeof(struct alpha_node *)); ++i)
        agent->alpha_nodes[i] = 0;

    agent->assertions = agent->retractions = 0;
}


void
rete_add_production(struct agent* agent, struct production* p)
{
    unsigned depth = 0;
    struct beta_node* parent = &agent->root_node;
    struct condition* cond;

    struct variable_binding_list* bindings = 0;

    /* Iterate through the conditions of the production, constructing the
       beta network as we do so. */
    for (cond = p->conditions; cond != 0; cond = cond->next) {
        struct beta_node* child = 0;

        switch (cond->type) {
        case condition_type_positive:
            child = ensure_positive_condition_node(agent, cond, ++depth, parent, &bindings);
            break;

        case condition_type_negative:
            child = ensure_negative_condition_node(agent, cond, ++depth, parent, &bindings);
            break;

        case condition_type_conjunctive_negation:
            UNIMPLEMENTED(); /* XXX write me */
            break;
        }

        parent = child;
    }


    {
        /* Build the production node at the tail of the party */
        struct action* action;

        struct beta_node* production_node =
            create_production_node(agent, parent, p);

        /* Convert any rhs_value's that are `variable' symbols into
           variable bindings */
        for (action = p->actions; action != 0; action = action->next) {
            process_rhs_value(&action->id, bindings, depth);
            process_rhs_value(&action->attr, bindings, depth);
            process_rhs_value(&action->value, bindings, depth);

            if (action->preference_type & preference_type_binary)
                process_rhs_value(&action->referent, bindings, depth);
        }

        initialize_matches(agent, production_node, parent);
    }

    while (bindings) {
        struct variable_binding_list* doomed = bindings;
        bindings = bindings->next;
        free(doomed);
    }
}

void
rete_remove_production()
{
    UNIMPLEMENTED();
}

void
rete_operate_wme(struct agent* agent, struct wme* wme, wme_operation_t op)
{
    int offset = (wme->type == wme_type_normal) ? 0 : 8;
    int i;

    for (i = 0; i < 8; ++i) {
        struct alpha_node* alpha;

        for (alpha = agent->alpha_nodes[i + offset]; alpha != 0; alpha = alpha->siblings) {
            if (wme_matches_alpha_node(wme, alpha)) {
                if (op == wme_operation_add) {
                    struct beta_node* beta;
                    add_wme_to_alpha_node(agent, alpha, wme);

                    for (beta = alpha->children; beta != 0; beta = beta->next_with_same_alpha_node)
                        do_right_addition(agent, beta, wme);
                }
                else {
                    struct beta_node* beta;
                    for (beta = alpha->children; beta != 0; beta = beta->next_with_same_alpha_node)
                        do_right_removal(agent, beta, wme);

                    remove_wme_from_alpha_node(agent, alpha, wme);
                }
            }
        }
    }
}

/*
 * Extract the bound symbol from a variable binding and a token.
 */
symbol_t
rete_get_variable_binding(variable_binding_t binding, struct token* token)
{
    int depth = (int) binding.depth;
    while (--depth >= 0)
        token = token->parent;

    return get_field_from_wme(token->wme, binding.field);
}


void
rete_finish(struct agent* agent)
{
    UNIMPLEMENTED();
}


