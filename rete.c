/*
 * rete.c
 */
#include "pool.h"
#include "wmem.h"
#include "rete.h"

static void
do_left_addition(struct rete* net, struct beta_node* node, struct token* token, struct wme* wme);

static void
do_right_addition(struct rete* net, struct beta_node* node, struct wme* wme);

struct variable_binding_list {
    symbol_t                      variable;
    variable_binding_t            binding;
    struct variable_binding_list* next;
};


static INLINE struct beta_node*
create_beta_node(struct rete* net)
{
    return (struct beta_node*) pool_alloc(&net->beta_node_pool);
}

/*
 * Create a new beta_test object from the beta_test pool
 */
static INLINE struct beta_test*
create_beta_test(struct rete* net)
{
    return (struct beta_test*) pool_alloc(&net->beta_test_pool);
}

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
            assert(0);
            break;

        case test_type_disjunctive:
            if (! beta_tests_are_identical(left->data.disjuncts, right->data.disjuncts))
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
                if (! SYMBOLS_ARE_EQUAL(left->data.constant_referent, right->data.constant_referent))
                    return 0;
            }
            else {
                if (! VARIABLE_BINDINGS_ARE_EQUAL(left->data.variable_referent, right->data.variable_referent))
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
free_beta_tests(struct rete* net, struct beta_test* tests)
{
    while (tests) {
        struct beta_test* doomed = tests;
        tests = tests->next;
        pool_free(&net->beta_test_pool, doomed);
    }
}

/*
 * Create a new variable binding list
 */
static INLINE struct variable_binding_list*
create_variable_binding_list(struct rete* net)
{
    return (struct variable_binding_list*) pool_alloc(&net->variable_binding_list_pool);
}

/*
 * Create a new token
 */
static INLINE struct token*
create_token(struct rete* net, struct beta_node* node, struct token* parent, struct wme* wme)
{
    struct token* result = (struct token*) pool_alloc(&net->token_pool);
    result->parent = parent;
    result->node = node;
    result->wme = wme;

    /* push onto list of tokens that are owned by `node' */
    result->next = node->tokens;
    node->tokens = result;

    return result;
}

/*
 * Given an id, attr, value, and alpha-type, determine which alpha
 * memory bucket a test should be in.
 */
static INLINE short
get_alpha_test_index(symbol_t id, symbol_t attr, symbol_t value, wme_type_t type)
{
    return ((type == wme_type_acceptable_preference) ? 8 : 0) +
        (GET_SYMBOL_VALUE(id) ? 4 : 0) +
        (GET_SYMBOL_VALUE(attr) ? 2 : 0) + 
        (GET_SYMBOL_VALUE(value) ? 1 : 0);
}

/*
 * Select a field from the specified wme.
 */
static INLINE symbol_t
get_field_from_wme(struct wme* wme, field_t field)
{
    assert(wme != 0);
    switch (field) {
    case field_id:     return wme->id;
    case field_attr:   return wme->attr;
    case field_value:  return wme->value;
    }
    assert(0); /* shouldn't get here */
}

/*
 * Determine if a working memory element matches an alpha node
 */
static INLINE bool_t
wme_matches_alpha_node(const struct wme* wme, const struct alpha_node* node)
{
    return (SYMBOL_IS_NIL(node->id) || SYMBOLS_ARE_EQUAL(node->id, wme->id)) &&
        (SYMBOL_IS_NIL(node->attr) || SYMBOLS_ARE_EQUAL(node->attr, wme->attr)) &&
        (SYMBOL_IS_NIL(node->value) || SYMBOLS_ARE_EQUAL(node->value, wme->value));
}

/*
 * Add a wme to the specfieid alpha node's right memory
 */
static void
add_wme_to_alpha_node(struct rete* net, struct alpha_node* node, struct wme* wme)
{
    struct right_memory* rm = (struct right_memory*) pool_alloc(&net->right_memory_pool);
    rm->wme = wme;
    rm->next_in_alpha_node = node->right_memories;
    node->right_memories = rm;
}

/*
 * Find an existing alpha node that is appropriate for testing the
 * specified fields.
 *
 * Corresponds to find_alpha_mem() in rete.c from Soar8.
 */
static struct alpha_node*
find_alpha_node(struct rete* net, symbol_t id, symbol_t attr, symbol_t value, wme_type_t type)
{
    struct alpha_node* node = net->alpha_nodes[get_alpha_test_index(id, attr, value, type)];
    for ( ; node != 0; node = node->siblings) {
        if (SYMBOLS_ARE_EQUAL(id, node->id) &&
            SYMBOLS_ARE_EQUAL(attr, node->attr) &&
            SYMBOLS_ARE_EQUAL(value, node->value))
            return node;
    }
    return 0;
}

/*
 * Find an existing alpha node that is appropriate for testing the
 * specified fields. If none exists, create a new one.
 *
 * This corresponds to find_or_make_alpha_mem() in rete.c from Soar8.
 */
static struct alpha_node*
ensure_alpha_node(struct rete* net, symbol_t id, symbol_t attr, symbol_t value, wme_type_t type)
{
    struct alpha_node* result;

    if (! (result = find_alpha_node(net, id, attr, value, type))) {
        struct alpha_node** head = &net->alpha_nodes[get_alpha_test_index(id, attr, value, type)];
        struct alpha_node* more_general_node;
        symbol_t nil;

        result = (struct alpha_node*) pool_alloc(&net->alpha_node_pool);
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
            more_general_node = find_alpha_node(net, nil, attr, value, type);

        if (! more_general_node && ! SYMBOL_IS_NIL(value))
            more_general_node = find_alpha_node(net, nil, attr, nil, type);

        if (more_general_node) {
            /* Found a more general working memory; use it to fill in
               our right memory */
            struct right_memory* rm;
            for (rm = more_general_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (wme_matches_alpha_node(rm->wme, result))
                    add_wme_to_alpha_node(net, result, rm->wme);
            }
        }
        else {
            /* Troll through *all* the wmes */
            struct wme* wme;
            for (wme = net->wmem->wmes; wme != 0; wme = GET_WME_NEXT(*wme)) {
                if (wme_matches_alpha_node(wme, result))
                    add_wme_to_alpha_node(net, result, wme);
            }
        }
    }

    return result;
}

/*
 * Look through a variable binding list to find the binding for the
 * specified variable.
 */
static INLINE const variable_binding_t*
find_bound_variable(const struct variable_binding_list* bindings, const symbol_t variable)
{
    while (bindings) {
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
bind_variables(struct rete* net,
               const struct test* test,
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
                create_variable_binding_list(net);

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
                bind_variables(net, &tests->test, depth, field, bindings);
        }
        break;

    case test_type_disjunctive:
        assert(0); /* XXX write me! */
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
process_test(struct rete* net,
             const struct test* test,
             unsigned depth,
             field_t field,
             const struct variable_binding_list* bindings,
             symbol_t* constant,
             struct beta_test** beta_tests)
{
    struct beta_test* beta_test = 0;

    if (test->type == test_type_blank)
        return;

    switch (test->type) {
    case test_type_equality:
    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
        if (GET_SYMBOL_TYPE(test->data.referent) == symbol_type_variable) {
            /* It's a variable. Make a variable relational test */
            const variable_binding_t* binding;
            beta_test = create_beta_test(net);

            binding = find_bound_variable(bindings, test->data.referent);
            assert(binding != 0);

            beta_test->relational_type = relational_type_variable;
            beta_test->data.variable_referent = *binding;

            /* Fix up the variable referent's depth (which was stored
               as an `absolute depth' in the binding list) to be
               relative to the current depth of the test. */
            beta_test->data.variable_referent.depth -= depth;
        }
        else {
            /* It's a constant. Install an alpha test if possible;
               otherwise, create a beta test node */
            if (! constant->val) {
                *constant = test->data.referent;
            }
            else {
                beta_test = create_beta_test(net);
                beta_test->relational_type = relational_type_constant;                
                beta_test->data.constant_referent = *constant;
            }
        }
        break;

    case test_type_goal_id:
    case test_type_impasse_id:
        beta_test = create_beta_test(net);
        break;

    case test_type_blank:
        /* do nothing */
        break;

    case test_type_conjunctive:
        {
            struct test_list* tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next)
                process_test(net, &tests->test, depth, field, bindings, constant, beta_tests);
        }
        break;

    case test_type_disjunctive:
        assert(0); /* XXX just hit a test that needs writin' */
    }

    if (beta_test) {
        beta_test->type  = test->type;
        beta_test->field = field;
        beta_test->next = *beta_tests;
        *beta_tests = beta_test;
    }
}

/*
 * Check a single beta test
 */
static bool_t
check_beta_test(struct rete* net, struct beta_test* test, struct token* token, struct wme* wme)
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
                        assert(0); /* never reached */
                    }
                }

                break;

            default:
                assert(0); /* never reached */
            }
        }
        break;

    case test_type_goal_id:
        {
            struct symbol_list* goal;
            for (goal = net->goals; goal != 0; goal = goal->next) {
                if (SYMBOLS_ARE_EQUAL(goal->symbol, wme->id))
                    return 1;
            }
        }
        break;

    case test_type_impasse_id:
        {
            struct symbol_list* impasse;
            for (impasse = net->impasses; impasse != 0; impasse = impasse->next) {
                if (SYMBOLS_ARE_EQUAL(impasse->symbol, wme->id))
                    return 1;
            }
        }
        break;

    case test_type_disjunctive:
        {
            struct beta_test* disjunct;
            for (disjunct = test->data.disjuncts; disjunct != 0; disjunct = disjunct->next) {
                if (check_beta_test(net, disjunct, token, wme))
                    return 1;
            }
        }
        break;

    case test_type_conjunctive:
    case test_type_blank:
        /* shouldn't ever hit this; conjunctive tests are
           converted into a list of single tests. */
        assert(0);
        break;
    }

    /* If we get here, the test failed */
    return 0;
}

/*
 * Check a list of beta tests
 */
static INLINE bool_t
check_beta_tests(struct rete* net, struct beta_test* test, struct token* token, struct wme* wme)
{
    for ( ; test != 0; test = test->next) {
        if (! check_beta_test(net, test, token, wme))
            return 0;
    }

    /* If we get here, all the tests passed. */
    return 1;
}

/*
 * Propogate matches from the parent node to its child.
 *
 * This corresponds to update_node_with_matches_from_above() from
 * rete.c in Soar8.
 */
static void
initialize_matches(struct rete* net, struct beta_node* child, struct beta_node* parent)
{
    if (parent->type == beta_node_type_root)
        do_left_addition(net, child, &net->root_token, 0);
    else if (parent->type & beta_node_type_bit_positive) {
        struct beta_node* old_children = parent->children;
        struct beta_node* old_siblings = child->siblings;
        struct right_memory* rm;

        parent->children = child;
        child->siblings = 0;

        for (rm = parent->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node)
            do_right_addition(net, parent, rm->wme);

        parent->children = old_children;
        child->siblings = old_siblings;
    }
    else {
        assert(0); /* XXX write me! */
    }
}

/*
 * Create a beta memory node with the specified parent.
 */
static struct beta_node*
create_memory_node(struct rete* net, struct beta_node* parent)
{
    struct beta_node* result;

    result = create_beta_node(net);
    result->type       = beta_node_type_memory;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;

    initialize_matches(net, result, parent);

    return result;
}

/*
 * Create a positive join node with the specified parent.
 */
static struct beta_node*
create_positive_join_node(struct rete* net,
                          struct beta_node* parent,
                          struct alpha_node* alpha_node,
                          struct beta_test* tests)
{
    struct beta_node* result;

    result = create_beta_node(net);
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

/*
 * Create a production node with the specified parent
 */
static struct beta_node*
create_production_node(struct rete* net,
                       struct beta_node* parent,
                       const struct production* production)
{
    struct beta_node* result;

    result = create_beta_node(net);
    result->type       = beta_node_type_production;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;
    result->data.production = production;

    return result;
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
ensure_positive_condition_node(struct rete* net,
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
    bind_variables(net, &cond->data.simple.id_test,    depth, field_id,    bindings);
    bind_variables(net, &cond->data.simple.attr_test,  depth, field_attr,  bindings);
    bind_variables(net, &cond->data.simple.value_test, depth, field_value, bindings);

    process_test(net, &cond->data.simple.id_test,    depth, field_id,    *bindings, &alpha_id,    &tests);
    process_test(net, &cond->data.simple.attr_test,  depth, field_attr,  *bindings, &alpha_attr,  &tests);
    process_test(net, &cond->data.simple.value_test, depth, field_value, *bindings, &alpha_value, &tests);

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
                ensure_alpha_node(net, alpha_id, alpha_attr, alpha_value, 0 /*XXX wme vs. acceptable*/);

            return create_positive_join_node(net, memory_node, alpha_node, tests);
        }

        /* Hey, we found one. Free the tests we allocated and return
           the node we found. */
        free_beta_tests(net, tests);
        return result;
    }

    /* XXX At this point, if we'd implemented `memory/positive-join'
       nodes, we'd want to grovel through parent's children to see if
       there was an MP node that we could split to produce a single
       memory node with two positive-join children. */

    /* No memory node, so create one and attach a positive join node to it. */
    memory_node = create_memory_node(net, parent);

    alpha_node =
        ensure_alpha_node(net, alpha_id, alpha_attr, alpha_value, 0 /*XXX wme vs. acceptable*/);

    return create_positive_join_node(net, memory_node, alpha_node, tests);
}

/* ---------------------------------------------------------------------- */

static void
do_left_addition(struct rete* net, struct beta_node* node, struct token* token, struct wme* wme)
{
    switch (node->type) {
    case beta_node_type_memory:
        {
            /* Add a new token to the memory and notify children */
            struct token* new_token = create_token(net, node, token, wme);
            struct beta_node* child;
            for (child = node->children; child != 0; child = child->siblings)
                do_left_addition(net, child, new_token, 0);
        }
        break;

    case beta_node_type_positive_join:
        {
            struct right_memory* rm;
            for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (check_beta_tests(net, node->data.tests, token, rm->wme)) {
                    struct beta_node* child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_addition(net, child, token, rm->wme);
                }
            }
        }
        break;

    case beta_node_type_production:
        {
            struct token* new_token = create_token(net, node, token, wme);
            /* XXX uh, add to match set buffer? */
        }
        break;

    case beta_node_type_memory_positive_join:
    case beta_node_type_negative:
    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
        assert(0); /* XXX write me! */
        break;

    case beta_node_type_root:
        assert(0); /* can't get left addition on this node */
        break;
    }
}


static void
do_right_addition(struct rete* net, struct beta_node* node, struct wme* wme)
{
    switch (node->type) {
    case beta_node_type_positive_join:
        {
            struct token* token;
            for (token = node->parent->tokens; token != 0; token = token->next) {
                if (check_beta_tests(net, node->data.tests, token, wme)) {
                    struct beta_node* child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_addition(net, child, token, wme);
                }
            }
        }
        break;

    case beta_node_type_memory_positive_join:
    case beta_node_type_negative:
        assert(0); /* XXX write me! */
        break;

    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
    case beta_node_type_root:
    case beta_node_type_production:
    case beta_node_type_memory:
        assert(0); /* can't get right addition on these nodes */
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
rete_init(struct rete* net, struct wmem* wmem)
{
    int i;

    net->root_node.type = beta_node_type_root;
    net->root_node.tokens = &net->root_token;

    net->root_token.parent = 0;
    net->root_token.node   = &net->root_node;
    net->root_token.wme    = 0;
    net->root_token.next   = 0;

    pool_init(&net->alpha_node_pool, sizeof(struct alpha_node), 8);
    pool_init(&net->right_memory_pool, sizeof(struct right_memory), 8);
    pool_init(&net->beta_node_pool, sizeof(struct beta_node), 8);
    pool_init(&net->beta_test_pool, sizeof(struct beta_test), 8);
    pool_init(&net->variable_binding_list_pool, sizeof(struct variable_binding_list), 8);
    pool_init(&net->token_pool, sizeof(struct token), 8);
    pool_init(&net->goal_impasse_pool, sizeof(struct symbol_list), 8);

    for (i = 0; i < (sizeof(net->alpha_nodes) / sizeof(struct alpha_node *)); ++i)
        net->alpha_nodes[i] = 0;

    net->wmem = wmem;
    net->goals = net->impasses = 0;
}


void
rete_add_production(struct rete* net, const struct production* p)
{
    unsigned depth = 0;
    struct beta_node* parent = &net->root_node;
    struct beta_node* production_node;
    struct condition* cond;

    struct variable_binding_list* bindings = 0;

    /* Iterate through the conditions of the production, constructing the
       beta network as we do so. */
    for (cond = p->lhs; cond != 0; cond = cond->next) {
        struct beta_node* child;

        switch (cond->type) {
        case condition_type_positive:
            child = ensure_positive_condition_node(net, cond, ++depth, parent, &bindings);
            break;

        case condition_type_negative:
        case condition_type_conjunctive_negation:
        default:
            child = 0;
            break;
        }

        assert(child != 0);
        parent = child;
    }

    production_node = create_production_node(net, parent, p);
    initialize_matches(net, production_node, parent);

    while (bindings) {
        struct variable_binding_list* doomed = bindings;
        bindings = bindings->next;
        pool_free(&net->variable_binding_list_pool, doomed);
    }
}

void
rete_remove_production()
{
}

void
rete_add_wme(struct rete* net, struct wme* wme)
{
    int offset;
    int i;

    offset = (GET_WME_TYPE(*wme) == wme_type_normal) ? 0 : 8;
    for (i = 0; i < 8; ++i) {
        struct alpha_node* alpha;

        for (alpha = net->alpha_nodes[i + offset]; alpha != 0; alpha = alpha->siblings) {
            if (wme_matches_alpha_node(wme, alpha)) {
                struct beta_node* beta;
                add_wme_to_alpha_node(net, alpha, wme);

                for (beta = alpha->children; beta != 0; beta = beta->next_with_same_alpha_node)
                    do_right_addition(net, beta, wme);
            }
        }
    }
}

void
rete_remove_wme()
{
}


void
rete_push_goal_id(struct rete* net, symbol_t goal_id)
{
    struct symbol_list* entry = (struct symbol_list*) pool_alloc(&net->goal_impasse_pool);
    entry->symbol = goal_id;
    entry->next = net->goals;
    net->goals = entry;
}

symbol_t
rete_pop_goal_id(struct rete* net)
{
    struct symbol_list* doomed = net->goals;
    symbol_t last;
    assert(doomed != 0);
    last = doomed->symbol;
    net->goals = doomed->next;
    pool_free(&net->goal_impasse_pool, doomed);
    return last;
}
