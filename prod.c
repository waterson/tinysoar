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
 * Copyright (C) 2000 Christopher R. Waterson. All Rights Reserved.
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
 * Routines used to build the RETE network from the raw Soar8
 * productions. This code is not part of the runtime (although some of
 * it may need to become part of the runtime when chunking is
 * implemented).
 *
 * This code is based on code from Soar8.2,
 *
 *   <http://ai.eecs.umich.edu/soar/>
 *
 * and includes some cross-references to similar routines in that
 * codebase.
 */

#include "soar.h"
#include "rete.h"
#include "alloc.h"

/*
 * Free a list of beta tests.
 */
static void
free_beta_tests(struct agent *agent, struct beta_test *tests)
{
    while (tests) {
        struct beta_test *doomed = tests;
        tests = tests->next;
        free(doomed);
    }
}

/*
 * Determine if two beta_test lists are identical. Kinda lame,
 * because it doesn't handle goofy ordering, but oh well.
 */
static bool_t
beta_tests_are_identical(struct beta_test *left, struct beta_test *right)
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
 * Find an existing alpha node that is appropriate for testing the
 * specified fields.
 *
 * Corresponds to find_alpha_mem() in rete.c from Soar8.
 */
static struct alpha_node *
find_alpha_node(struct agent *agent,
                symbol_t      id,
                symbol_t      attr,
                symbol_t      value,
                wme_type_t    type)
{
    struct alpha_node *node =
        agent->alpha_nodes[get_alpha_test_index(id, attr, value, type)];

    for ( ; node != 0; node = node->siblings) {
        if (SYMBOLS_ARE_EQUAL(id, node->id) &&
            SYMBOLS_ARE_EQUAL(attr, node->attr) &&
            SYMBOLS_ARE_EQUAL(value, node->value))
            return node;
    }
    return 0;
}

/*
 * Add existing WMEs to the alpha node when a new alpha node is created.
 */
static void
add_matching_wmes(struct agent *agent, struct wme *wme, void *closure)
{
    struct alpha_node *alpha_node = (struct alpha_node *) closure;
    if (wme_matches_alpha_node(wme, alpha_node))
        add_wme_to_alpha_node(agent, alpha_node, wme);
}

/*
 * Find an existing alpha node that is appropriate for testing the
 * specified fields. If none exists, create a new one.
 *
 * This corresponds to find_or_make_alpha_mem() in rete.c from Soar8.
 */
static struct alpha_node *
ensure_alpha_node(struct agent *agent,
                  symbol_t      id,
                  symbol_t      attr,
                  symbol_t      value,
                  wme_type_t    type)
{
    struct alpha_node *result;

    if (! (result = find_alpha_node(agent, id, attr, value, type))) {
        struct alpha_node **head =
            &agent->alpha_nodes[get_alpha_test_index(id, attr, value, type)];

        struct alpha_node *more_general_node;
        symbol_t nil;

        result = (struct alpha_node *) malloc(sizeof(struct alpha_node));
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
            struct right_memory *rm;
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
 * Propagate matches from the parent node to its child.
 *
 * This corresponds to update_node_with_matches_from_above() from
 * rete.c in Soar8.
 */
static void
initialize_matches(struct agent     *agent,
                   struct beta_node *child,
                   struct beta_node *parent)
{
    if (parent->type == beta_node_type_root) {
        do_left_addition(agent, child, &agent->root_token, 0);
    }
    else if (parent->type & beta_node_type_bit_positive) {
        /* Temporarily splice out all of parent's children except
           `child', then call the right-addition routine, and restore
           parent's children. */
        struct beta_node *old_children = parent->children;
        struct beta_node *old_siblings = child->siblings;
        struct right_memory *rm;

        parent->children = child;
        child->siblings = 0;

        for (rm = parent->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node)
            do_right_addition(agent, parent, rm->wme);

        parent->children = old_children;
        child->siblings = old_siblings;
    }
    else if (parent->type & beta_node_type_bit_negative) {
        struct token *token;
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
static struct beta_node *
create_memory_node(struct agent *agent, struct beta_node *parent)
{
    struct beta_node *result;

    result = (struct beta_node *) malloc(sizeof(struct beta_node));
    result->type       = beta_node_type_memory;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;
    result->alpha_node = 0;
    result->next_with_same_alpha_node = 0;
    result->tokens     = 0;

    initialize_matches(agent, result, parent);

    return result;
}

/*
 * Create a positive join node with the specified parent.
 */
static struct beta_node *
create_positive_join_node(struct agent      *agent,
                          struct beta_node  *parent,
                          struct alpha_node *alpha_node,
                          struct beta_test  *tests)
{
    struct beta_node *result =
        (struct beta_node *) malloc(sizeof(struct beta_node));

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
 * Create a negative node with the specified parent.
 */
static struct beta_node *
create_negative_node(struct agent      *agent,
                     struct beta_node  *parent,
                     struct alpha_node *alpha_node,
                     struct beta_test  *tests)
{
    struct beta_node *result =
        (struct beta_node *) malloc(sizeof(struct beta_node));

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
static struct beta_node *
create_production_node(struct agent      *agent,
                       struct beta_node  *parent,
                       struct production *production)
{
    struct beta_node *result;

    result = (struct beta_node *) malloc(sizeof(struct beta_node));
    result->type       = beta_node_type_production;
    result->parent     = parent;
    result->siblings   = parent->children;
    parent->children   = result;
    result->children   = 0;
    result->alpha_node = 0;
    result->next_with_same_alpha_node = 0;
    result->tokens     = 0;
    result->data.production = production;

    /* XXX why not? */
    /* initialize_matches(agent, result, parent); */

    return result;
}

/*
 * Look through a variable binding list to find the binding for the
 * specified variable.
 */
static inline const variable_binding_t *
find_bound_variable(const struct variable_binding_list *bindings,
                    const symbol_t                      variable)
{
    for ( ; bindings != 0; bindings = bindings->next) {
        if (SYMBOLS_ARE_EQUAL(bindings->variable, variable))
            return &bindings->binding;
    }

    return 0;
}

/*
 * Convert a `test' from a condition into the appropriate RETE
 * structures: constants that are tested by the alpha network and
 * nodes in the beta network.
 *
 * This corresponds to add_rete_tests_for_test() from rete.c in Soar8.
 */
static void
process_test(const struct test                  *test,
             unsigned                            depth,
             field_t                             field,
             const struct variable_binding_list *bindings,
             symbol_t                           *constant,
             struct beta_test                  **beta_tests,
             bool_t                              force_beta_test)
{
    struct beta_test *beta_test = 0;
    symbol_type_t symbol_type;

    if (test->type == test_type_blank)
        return;

    symbol_type = GET_SYMBOL_TYPE(test->data.referent);

    switch (test->type) {
    case test_type_equality:
        if (!force_beta_test && symbol_type != symbol_type_variable
            && !GET_SYMBOL_VALUE(*constant)) {
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
            const variable_binding_t *binding;
            int relative_depth;

            binding = find_bound_variable(bindings, test->data.referent);
            ASSERT(binding != 0, ("null ptr"));

            relative_depth = depth - GET_VARIABLE_BINDING_DEPTH(*binding);

            if (relative_depth || GET_VARIABLE_BINDING_FIELD(*binding) != field) {
                /* Don't bother to make vacuous tests. */
                beta_test = (struct beta_test *) malloc(sizeof(struct beta_test));
                beta_test->relational_type = relational_type_variable;
                beta_test->data.variable_referent = *binding;

                /* Fix up the variable referent's depth (which was stored
                   as an `absolute depth' in the binding list) to be
                   relative to the current depth of the test. */
                SET_VARIABLE_BINDING_DEPTH(beta_test->data.variable_referent, relative_depth);
            }
        }
        else {
            beta_test =
                (struct beta_test *) malloc(sizeof(struct beta_test));

            beta_test->relational_type = relational_type_constant;
            beta_test->data.constant_referent = test->data.referent;
        }
        break;

    case test_type_goal_id:
        beta_test = (struct beta_test *) malloc(sizeof(struct beta_test));
        beta_test->relational_type = relational_type_constant;
        CLEAR_SYMBOL(beta_test->data.constant_referent);
        break;

    case test_type_blank:
        /* do nothing */
        break;

    case test_type_conjunctive:
        {
            struct test_list *tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next) {
                process_test(&tests->test, depth, field, bindings, constant,
                             beta_tests, 0);
            }
        }
        break;

    case test_type_disjunctive:
        {
            struct test_list *tests;

            beta_test = (struct beta_test *) malloc(sizeof(struct beta_test));
            beta_test->data.disjuncts = 0;

            for (tests = test->data.disjuncts; tests != 0; tests = tests->next) {
                process_test(&tests->test, depth, field, bindings, constant,
                             &beta_test->data.disjuncts, 1);
            }
        }
        break;

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
bind_variables(const struct test             *test,
               unsigned                       depth,
               field_t                        field,
               struct variable_binding_list **bindings)
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
            struct variable_binding_list *entry =
                (struct variable_binding_list *)
                malloc(sizeof(struct variable_binding_list));

            entry->variable = test->data.referent;
            INIT_VARIABLE_BINDING(entry->binding, field, depth);
            entry->next = *bindings;
            *bindings = entry;
        }
        break;

    case test_type_conjunctive:
    case test_type_disjunctive:
        {
            struct test_list *tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next)
                bind_variables(&tests->test, depth, field, bindings);
        }
        break;

    case test_type_goal_id:
    case test_type_blank:
        /* XXX do nothing? */
        break;
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
static struct beta_node *
ensure_positive_condition_node(struct agent                  *agent,
                               const struct condition        *cond,
                               unsigned                       depth,
                               struct beta_node              *parent,
                               struct variable_binding_list **bindings)
{
    struct beta_node *result;
    struct beta_node *memory_node;
    struct beta_test *tests = 0;
    symbol_t alpha_id, alpha_attr, alpha_value;
    struct alpha_node *alpha_node;

    CLEAR_SYMBOL(alpha_id);
    CLEAR_SYMBOL(alpha_attr);
    CLEAR_SYMBOL(alpha_value);

    bind_variables(&cond->data.simple.id_test,    depth, field_id,    bindings);
    bind_variables(&cond->data.simple.attr_test,  depth, field_attr,  bindings);
    bind_variables(&cond->data.simple.value_test, depth, field_value, bindings);

    process_test(&cond->data.simple.id_test, depth, field_id, *bindings,
                 &alpha_id, &tests, 0);

    process_test(&cond->data.simple.attr_test, depth, field_attr, *bindings,
                 &alpha_attr, &tests, 0);

    process_test(&cond->data.simple.value_test, depth, field_value, *bindings,
                 &alpha_value, &tests, 0);

    /* See if there's a memory node we can use. */
    for (memory_node = parent->children; memory_node != 0; memory_node = memory_node->siblings) {
        if (memory_node->type == beta_node_type_memory)
            break;
    }

    if (memory_node) {
        /* Now look for a positive-join node we can use. It ought to
           have the same alpha node that we'd get, as well as
           identical beta tests. */
        for (result = memory_node->children; result != 0; result = result->siblings) {
            if ((result->type == beta_node_type_positive_join)
                && (find_alpha_node(agent, alpha_id, alpha_attr,
                                    alpha_value, cond->acceptable)
                    == result->alpha_node)
                && beta_tests_are_identical(result->data.tests, tests)) {
                break;
            }
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
static struct beta_node *
ensure_negative_condition_node(struct agent                  *agent,
                               const struct condition        *cond,
                               unsigned                       depth,
                               struct beta_node              *parent,
                               struct variable_binding_list **bindings)
{
    struct beta_node *result;
    struct beta_test *tests = 0;
    symbol_t alpha_id, alpha_attr, alpha_value;
    struct alpha_node *alpha_node;

    CLEAR_SYMBOL(alpha_id);
    CLEAR_SYMBOL(alpha_attr);
    CLEAR_SYMBOL(alpha_value);

    bind_variables(&cond->data.simple.id_test,    depth, field_id,    bindings);
    bind_variables(&cond->data.simple.attr_test,  depth, field_attr,  bindings);
    bind_variables(&cond->data.simple.value_test, depth, field_value, bindings);

    process_test(&cond->data.simple.id_test, depth, field_id, *bindings,
                 &alpha_id, &tests, 0);

    process_test(&cond->data.simple.attr_test, depth, field_attr, *bindings,
                 &alpha_attr,  &tests, 0);

    process_test(&cond->data.simple.value_test, depth, field_value, *bindings,
                 &alpha_value, &tests, 0);

    /* See if there's already a negative node we can use. It ought to
       have the same alpha node that we'd get, as well as identical
       beta tests. */
    for (result = parent->children; result != 0; result = result->siblings) {
        if ((result->type == beta_node_type_negative)
            && (find_alpha_node(agent, alpha_id, alpha_attr,
                                alpha_value, cond->acceptable)
                == result->alpha_node)
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

/*
 * Convert variable symbols into the corresponding variable binding.
 */
static void
process_rhs_value(struct rhs_value             *value,
                  struct variable_binding_list *bindings,
                  unsigned                      depth)
{
    if ((value->type == rhs_value_type_symbol) &&
        (GET_SYMBOL_TYPE(value->val.symbol) == symbol_type_variable)) {
        /* Look up the binding for the variable. There'd better be
           one, or this trip'll end real quick. */
        const variable_binding_t *binding =
            find_bound_variable(bindings, value->val.symbol);

        ASSERT(binding != 0, ("null ptr"));

        /* Replace the symbol with an the variable binding */
        value->type = rhs_value_type_variable_binding;
        value->val.variable_binding = *binding;

        /* Fix the depth (which was stored as an `absolute depth' in
           the binding list) to be relative to the current depth of
           the node in the rete network. */
        depth -= GET_VARIABLE_BINDING_DEPTH(value->val.variable_binding);
        SET_VARIABLE_BINDING_DEPTH(value->val.variable_binding, depth);
    }
}

/*
 * Add the variables from a single test to the list of visited variables
 */
static void
add_variables_from_test(struct test *test, struct symbol_list **visited)
{
    switch (test->type) {
    case test_type_blank:
        return;

    case test_type_equality:
    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
    case test_type_goal_id:
        if (GET_SYMBOL_TYPE(test->data.referent) == symbol_type_variable) {
            struct symbol_list *entry;

            /* Have we already added this variable to the visited list? */
            for (entry = *visited; entry != 0; entry = entry->next) {
                if (SYMBOLS_ARE_EQUAL(test->data.referent, entry->symbol))
                    break;
            }

            if (! entry) {
                /* Nope. */
                entry = (struct symbol_list *) malloc(sizeof(struct symbol_list));

                entry->symbol = test->data.referent;
                entry->next = *visited;
                *visited = entry;
            }
        }
        break;

    case test_type_conjunctive:
    case test_type_disjunctive:
        {
            struct test_list *tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next)
                add_variables_from_test(&tests->test, visited);
        }
        break;
    }
}

/*
 * Add the variables from a condition to the list of visited variables
 */
static void
add_variables_from(struct condition *cond, struct symbol_list **visited)
{
    ASSERT(cond->type == condition_type_positive, ("wrong type of condition"));
    add_variables_from_test(&cond->data.simple.id_test, visited);
    add_variables_from_test(&cond->data.simple.attr_test, visited);
    add_variables_from_test(&cond->data.simple.value_test, visited);
}

/*
 * Determine if all the variables in the specified test have been visited
 */
static bool_t
all_variables_visited_in_test(struct test *test, struct symbol_list *visited)
{
    switch (test->type) {
    case test_type_blank:
        break;

    case test_type_equality:
    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
    case test_type_goal_id:
        if (GET_SYMBOL_TYPE(test->data.referent) == symbol_type_variable) {
            while (visited) {
                if (SYMBOLS_ARE_EQUAL(visited->symbol, test->data.referent))
                    return 1;

                visited = visited->next;
            }
                
            return 0;
        }
        break;

    case test_type_conjunctive:
    case test_type_disjunctive:
        {
            struct test_list *tests;
            for (tests = test->data.conjuncts; tests != 0; tests = tests->next) {
                if (! all_variables_visited_in_test(&tests->test, visited))
                    return 0;
            }
        }
        break;
    }

    return 1;
}

/*
 * Determine if all the variables in the specified condition have been visited
 */
static bool_t
all_variables_visited_in(struct condition *cond, struct symbol_list *visited)
{
    return all_variables_visited_in_test(&cond->data.simple.id_test, visited)
        && all_variables_visited_in_test(&cond->data.simple.attr_test, visited)
        && all_variables_visited_in_test(&cond->data.simple.value_test, visited);
}

/*
 * Reorder the conditions of a production such that each negative
 * condition appears after the positive conditions that bind variables
 * which it refers to.
 */
static void
reorder_conditions(struct production *p)
{
    struct condition **link = &p->conditions;
    struct condition *cond = *link;
    struct condition *deferred = 0;
    struct symbol_list *visited = 0;

    while (cond) {
        switch (cond->type) {
        case condition_type_positive:
            {
                struct condition **dlink = &deferred;
                struct condition *d = *dlink;

                add_variables_from(cond, &visited);

                while (d) {
                    /* For each deferred negative condition, see if
                       we've now visited all the variables. If so, we
                       can re-insert the condition into the condition
                       list. */
                    if (all_variables_visited_in(d, visited)) {
                        *dlink = d->next;

                        /* Insert the deferred condition _after_ the
                           positive condition we just processed */
                        d->next = cond->next;
                        cond->next = d;

                        /* XXX we'll re-process each of the negative
                           conditions we've re-inserted the next time
                           through the loop. Oh well. */
                    }
                    else {
                        dlink = &d->next;
                    }

                    d = *dlink;
                }
            }
            break;
            
        case condition_type_negative:
            if (! all_variables_visited_in(cond, visited)) {
                /* We haven't visited all of the variables in the
                   negative condition, so it's possible that we might
                   have a subsequent positive condition that binds the
                   variable to a real value. Push the condition onto
                   the ``deferred'' list */
                *link = cond->next;

                cond->next = deferred;
                deferred = cond;

                cond = *link;
                continue;
            }
            break;

        case condition_type_conjunctive_negation:
            UNIMPLEMENTED();
            break;
        }

        link = &cond->next;
        cond = *link;
    }

    *link = deferred;

    while (visited) {
        struct symbol_list *doomed = visited;
        visited = visited->next;
        free(doomed);
    }
}

/*
 * Add a production to the RETE network.
 */
void
rete_add_production(struct agent *agent, struct production *p)
{
    unsigned depth = 0;
    struct beta_node *parent = agent->root_node;
    struct condition *cond;

    struct variable_binding_list *bindings = 0;

    /* Reorder conditions, demoting dependent negative conditions
       after their dependencies */
    reorder_conditions(p);

    /* Iterate through the conditions of the production, constructing the
       beta network as we do so. */
    for (cond = p->conditions; cond != 0; cond = cond->next) {
        struct beta_node *child = 0;

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
        struct action *action;

        struct beta_node *production_node =
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
        struct variable_binding_list *doomed = bindings;
        bindings = bindings->next;
        free(doomed);
    }
}

/*
 * Remove a production from the RETE network.
 */
void
rete_remove_production()
{
    UNIMPLEMENTED();
}

