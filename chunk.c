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
 * TODO
 *
 * . Handle justifications; they're currently variablized, which makes
 *   them over-general.
 *
 * . Optimize network creation.
 */

#include "soar.h"
#include "rete.h"
#include "alloc.h"

#ifdef DEBUG
#include <stdio.h>
#endif

struct preference_list {
    struct preference      *preference;
    struct preference_list *next;
};

struct token_list {
    struct token      *token;
    struct token_list *next;
};

struct instantiation_list {
    struct instantiation      *inst;
    struct instantiation_list *next;
};

struct chunk {
    struct token_list         *grounds;
    struct token_list         *potentials;
    struct token_list         *nots;
    struct instantiation_list *visited;
    int                        level;
    bool_t                     justification;
};

/*
 * Find the variable binding information for the specified
 * variable. Returns `null' if the variable isn't bound.
 */
static variable_binding_t *
get_variable_binding(struct variable_binding_list *bindings,
                     symbol_t                      variable)
{
    for ( ; bindings != 0; bindings = bindings->next) {
        if (SYMBOLS_ARE_EQUAL(bindings->variable, variable))
            return &bindings->binding;
    }

    return 0;
}

/*
 * Ensure that the specified variable has a binding, creating one with
 * the specified field and absolute depth information, if necessary.
 */
static variable_binding_t *
ensure_variable_binding(struct variable_binding_list **bindings,
                        symbol_t                       variable,
                        field_t                        field,
                        int                            depth)
{
    /* See if we've already bound this identifier. */
    variable_binding_t *binding =
        get_variable_binding(*bindings, variable);

    if (! binding) {
        /* Nope. Allocate a new binding and add it to the set. */
        struct variable_binding_list *list;

        list = (struct variable_binding_list *)
            malloc(sizeof(struct variable_binding_list));
                
        list->variable = variable;
        INIT_VARIABLE_BINDING(list->binding, field, depth);
        list->next = *bindings;
        *bindings = list;

        binding = &list->binding;
    }

    return binding;
}

/*
 * Copy the source tests to the destination, creating appropriate
 * variable bindings in the destination test.
 */
static void
copy_tests(struct beta_test             **dest,
           struct beta_test              *src,
           struct variable_binding_list **bindings,
           int                            depth,
           struct token                  *token)
{
    struct beta_test *test;

    for ( ; src != 0; src = src->next) {
        test = (struct beta_test *) malloc(sizeof(struct beta_test));

        /* XXX this could be more efficient; we probably could just
           bulk assign `src' to `test', and then fix up the other
           fields we need to. Will the compiler generate an implicit
           memcpy? */
        test->type = src->type;
        test->next = *dest;

        *dest = test;

        switch (test->type) {
        case test_type_equality:
        case test_type_not_equal:
        case test_type_less:
        case test_type_greater:
        case test_type_less_or_equal:
        case test_type_greater_or_equal:
        case test_type_same_type:
            test->relational_type = src->relational_type;
            test->field           = src->field;

            if (test->relational_type == relational_type_variable) {
                /* Convert the variable bindings from the source test
                   into variable bindings that are equivalent in the
                   destination test. */
                variable_binding_t *binding;
                symbol_t variable;
                int relative_depth;

                variable = rete_get_variable_binding(src->data.variable_referent, token);

                binding =
                    ensure_variable_binding(bindings, variable,
                                            test->field, depth);

                test->data.variable_referent = *binding;

                /* Convert the depth to be relative. */
                relative_depth =
                    depth - GET_VARIABLE_BINDING_DEPTH(test->data.variable_referent);

                if (relative_depth == 0 &&
                    GET_VARIABLE_BINDING_FIELD(test->data.variable_referent) == test->field) {
                    /* Don't create vacuous tests; i.e., that test the
                       same field at the same level. */
                    *dest = test->next;
                    free(test);
                }
                else {
                    SET_VARIABLE_BINDING_DEPTH(test->data.variable_referent,
                                               relative_depth);
                }
            }
            else
                test->data.constant_referent = src->data.constant_referent;

            break;

        case test_type_disjunctive:
            /* Recursively copy the disjuncts. */
            test->data.disjuncts = 0;
            copy_tests(&test->data.disjuncts, src->data.disjuncts,
                       bindings, depth, token);
            break;

        case test_type_conjunctive:
        case test_type_blank:
            ERROR(("unexpected test"));
            /* Fall through here to make case generation easier on the
               compiler. */

        case test_type_goal_id:
            /* No other information needed. */
            break;
        }
    }
}

/*
 * Returns `true' if any of the tests bind the `id' field to an
 * identifier from a higher level.
 */
static bool_t
tests_check_superstate(struct agent *agent, struct beta_test *tests, struct token *token, int level)
{
    for ( ; tests != 0; tests = tests->next) {
        switch (tests->type) {
        case test_type_equality:
        case test_type_not_equal:
        case test_type_less:
        case test_type_greater:
        case test_type_less_or_equal:
        case test_type_greater_or_equal:
        case test_type_same_type:
            if (tests->field == field_id && tests->relational_type == relational_type_variable) {
                symbol_t id = rete_get_variable_binding(tests->data.variable_referent, token);
                if (agent_get_id_level(agent, id) < level)
                    return 1;
            }
            break;

        case test_type_goal_id:
            break;

        case test_type_disjunctive:
            /* Recursively test the disjuncts. */
            if (tests_check_superstate(agent, tests->data.disjuncts, token, level))
                return 1;

            break;

        case test_type_conjunctive:
        case test_type_blank:
            /* We shouldn't encounter these types of tests. */
            UNREACHABLE();
        }
    }

    return 0;
}

/*
 * Fill in an rhs_value for the specified variable.
 */
static void
make_rhs_value(struct rhs_value              *value,
               symbol_t                       variable,
               int                            depth,
               struct variable_binding_list  *bindings,
               struct symbol_list           **unbound_variables)
{
    switch (GET_SYMBOL_TYPE(variable)) {
    case symbol_type_symbolic_constant:
    case symbol_type_integer_constant:
        value->type = rhs_value_type_symbol;
        value->val.symbol = variable;
        return;
        
    case symbol_type_variable:
        ERROR(("unexpected symbol type"));
        /* Fall through, to make life easier on the compiler. */

    case symbol_type_identifier:
        break;
    }

    /* See if it's a bound variable. */
    for ( ; bindings != 0; bindings = bindings->next) {
        if (SYMBOLS_ARE_EQUAL(bindings->variable, variable)) {
            int relative_depth;
            value->type = rhs_value_type_variable_binding;
            value->val.variable_binding = bindings->binding;
            relative_depth = depth - GET_VARIABLE_BINDING_DEPTH(value->val.variable_binding);
            SET_VARIABLE_BINDING_DEPTH(value->val.variable_binding, relative_depth);
            break;
        }
    }

    if (! bindings) {
        /* If we didn't find a binding, then it's an unbound
           variable. */
        struct symbol_list *unbound, **link;
        int index;

        value->type = rhs_value_type_unbound_variable;

        /* Is it one we've seen already? */
        for (link = unbound_variables, index = 0;
             (unbound = *link) != 0;
             link = &unbound->next, ++index) {
            if (SYMBOLS_ARE_EQUAL(unbound->symbol, variable))
                break;
        }

        if (! unbound) {
            /* Nope. Allocate a new entry. */
            unbound = (struct symbol_list *) malloc(sizeof(struct symbol_list));
            unbound->symbol = variable;
            unbound->next = 0;
            *link = unbound;
        }

        value->val.unbound_variable = index;
    }
}

/*
 * Build the right-hand side of the production.
 */
static void
make_rhs(struct agent                 *agent,
         struct beta_node             *parent,
         int                           depth,
         struct variable_binding_list *bindings,
         struct preference_list       *results,
         support_type_t                support,
         bool_t                        justification)
{
    struct production *production;
    struct beta_node *node;
    struct symbol_list *unbound_vars;

    /* Create a production structure. */
    production = (struct production *) malloc(sizeof(struct production));
    production->support        = support;
    production->instantiations = 0;
    production->actions        = 0;
    production->justification  = justification;

    /* Turn the chunk's results into the production's actions. */
    unbound_vars = 0;
    for ( ; results != 0; results = results->next) {
        struct preference *pref = results->preference;
        struct action *action;

        action = (struct action *) malloc(sizeof(struct action));
        action->preference_type = pref->type;
        make_rhs_value(&action->id, pref->slot->id, depth, bindings, &unbound_vars);
        make_rhs_value(&action->attr, pref->slot->attr, depth, bindings, &unbound_vars);
        make_rhs_value(&action->value, pref->value, depth, bindings, &unbound_vars);
        if (action->preference_type & preference_type_binary)
            make_rhs_value(&action->referent, pref->referent, depth, bindings, &unbound_vars);

        action->next = production->actions;
        production->actions = action;
    }

    /* Count the number of unbound variables. */
    production->num_unbound_vars = 0;

    while (unbound_vars) {
        struct symbol_list *doomed = unbound_vars;
        unbound_vars = unbound_vars->next;
        free(doomed);

        ++production->num_unbound_vars;
    }

#ifdef DEBUG
    {
        /* Give the chunk a name. */
        static int number = 0;
        char buf[24];
        sprintf(buf, "chunk-%d", ++number);
        production->name = strdup(buf);
    }
#endif

    /* Create a production beta node. */
    node = (struct beta_node *) malloc(sizeof(struct beta_node));
    node->type = beta_node_type_production;
    node->data.production = production;
    node->alpha_node = 0;
    node->next_with_same_alpha_node = 0;
    node->tokens = 0;
    node->parent = parent;
    node->siblings = parent->children;
    node->children = 0;
    parent->children = node;

    /* Create tokens! */
    rete_initialize_matches(agent, node, parent);
}

/*
 * Create and initialize a memory node with the specified parent.
 */
static struct beta_node *
create_memory_node(struct beta_node *parent)
{
    struct beta_node *memory = (struct beta_node *) malloc(sizeof(struct beta_node));
    memory->type = beta_node_type_memory;
    memory->alpha_node = 0;
    memory->tokens = 0;
    memory->parent = parent;
    memory->children = 0;
    memory->siblings = parent->children;
    parent->children = memory;
    return memory;
}

/*
 * If the specified variable is already bound, then add a test that
 * ensures we have a consistent binding in the beta network. If this
 * is the first time we've seen the variable, then just add it to the
 * set of bindings.
 */
static void
ensure_consistent_test(struct beta_test             **tests,
                       struct variable_binding_list **bindings,
                       symbol_t                       variable,
                       field_t                        field,
                       int                            depth)
{
    struct beta_test *test;
    int relative_depth;

    if (get_variable_binding(*bindings, variable)) {
        test = (struct beta_test *) malloc(sizeof(struct beta_test));
        test->type            = test_type_equality;
        test->relational_type = relational_type_variable;
        test->field           = field;
        test->data.variable_referent =
            *ensure_variable_binding(bindings, variable, field, depth);

        relative_depth = 
            depth - GET_VARIABLE_BINDING_DEPTH(test->data.variable_referent);

        SET_VARIABLE_BINDING_DEPTH(test->data.variable_referent, relative_depth);

        test->next = *tests;
        *tests = test;
    }
    else
        ensure_variable_binding(bindings, variable, field, depth);
}

/*
 * Add a goal test to the list of tests.
 */
static void
add_goal_test(struct beta_test **tests)
{
    struct beta_test *test =
        (struct beta_test *) malloc(sizeof(struct beta_test));

    test->type  = test_type_goal_id;
    test->field = field_id;
    test->next  = *tests;

    *tests = test;
}

/*
 * Create a negative condition node.
 */
static struct beta_node *
create_negative_node(struct agent                  *agent,
                     struct chunk                  *chunk,
                     struct variable_binding_list **bindings,
                     struct token                  *token,
                     struct beta_node              *parent,
                     int                            depth)
{
    struct beta_node *orig, *node;

    /* Create a negative node and link it into the beta network. */
    node = (struct beta_node *) malloc(sizeof(struct beta_node));
    node->type     = beta_node_type_negative;
    node->blocked  = 0;
    node->tokens   = 0;
    node->children = 0;
    node->parent   = parent;
    node->siblings = parent->children;

    parent->children = node;

    orig = token->node;

    /* Link it into the alpha network. */
    node->alpha_node = orig->parent->alpha_node;
    if (node->alpha_node) {
        node->next_with_same_alpha_node = node->alpha_node->children;
        node->alpha_node->children = node;
    }
    else
        node->next_with_same_alpha_node = 0;

    /* Copy the tests. We'd better not be adding any new
       bindings in a negated test, so we won't bother forcing
       the referents to be bound.
       XXX it'd be nice to assert that. */
    node->data.tests = 0;
    copy_tests(&node->data.tests, orig->parent->data.tests, bindings,
               depth, token);

    /* We'd better not be the first test that's testing a
       goal, either.
       XXX it'd be nice to assert that. */

    /* Propagate tokens downward. */
    rete_initialize_matches(agent, node, parent);

    return node;
}

/*
 * Build the production from the grounds, the `nots', and the results.
 */
static void
make_production(struct agent           *agent,
                struct chunk           *chunk,
                struct preference_list *results)
{
    struct variable_binding_list *bindings = 0;
    struct beta_node *parent = agent->root_node;
    struct token_list *tokens;
    int depth = 0;
    bool_t tested_goal = 0;
    support_type_t support = support_type_isupport;

    /* Create beta nodes for each token. */
    for (tokens = chunk->grounds; tokens != 0; ) {
        struct token_list *doomed;
        struct beta_node *memory = 0;
        struct beta_node *orig = tokens->token->node;
        struct beta_node *node;

        /* Create a memory node that will be the parent. */
        memory = create_memory_node(parent);

        /* Create a postive join node and link it into the beta network. */
        node = (struct beta_node *) malloc(sizeof(struct beta_node));

        node->type     = beta_node_type_positive_join;
        node->tokens   = 0;
        node->children = 0;
        node->parent   = memory;
        node->siblings = memory->children;

        memory->children = node;

        if (! orig) {
            /* This is a dummy token created for an ^item. */
            struct alpha_node *alpha_node;
            struct wme *wme;

            /* Find the (* ^operator *) alpha node, and link the
               positive join node to it. */
            alpha_node =
                rete_find_alpha_node(agent, 0, OPERATOR_CONSTANT, 0, wme_type_acceptable);

            node->alpha_node = alpha_node;
            node->next_with_same_alpha_node = alpha_node->children;
            alpha_node->children = node;

            /* Set up the beta tests. */
            wme = tokens->token->wme;

            node->data.tests = 0;

            ensure_consistent_test(&node->data.tests, &bindings,
                                   wme->slot->id, field_id, depth);

            ensure_consistent_test(&node->data.tests, &bindings,
                                   wme->value, field_value, depth);

            if (! tested_goal) {
                add_goal_test(&node->data.tests);
                ensure_variable_binding(&bindings, tokens->token->wme->slot->id, field_id, depth);
                tested_goal = 1;
            }

            ASSERT(node->data.tests != 0, ("uh, no tests!"));
        }
        else {
            ASSERT((orig->parent->type == beta_node_type_positive_join),
                   ("expected a positive join node"));

            /* Link it into the alpha network, if appropriate. */
            node->alpha_node = orig->parent->alpha_node;
            if (node->alpha_node) {
                node->next_with_same_alpha_node = node->alpha_node->children;
                node->alpha_node->children = node;
            }
            else
                node->next_with_same_alpha_node = 0;

            /* Copy the tests. XXX do we need field_attr, to? */
            ensure_variable_binding(&bindings, tokens->token->wme->slot->id, field_id, depth);

            if (GET_SYMBOL_TYPE(tokens->token->wme->value) == symbol_type_identifier)
                ensure_variable_binding(&bindings, tokens->token->wme->value, field_value, depth);

            node->data.tests = 0;
            copy_tests(&node->data.tests, orig->parent->data.tests, &bindings,
                       depth, tokens->token);

            /* If the `id' field is a goal, and we haven't yet added a
               test for the goal, then make one now. */
            if (! tested_goal && agent_is_goal(agent, tokens->token->wme->slot->id)) {
                add_goal_test(&node->data.tests);
                tested_goal = 1;
            }

            /* Compute o-support: if we test the `^operator' attribute
               of a wme in the grounds, we'll call it o-supported. */
            if ((support == support_type_isupport)
                && SYMBOLS_ARE_EQUAL(tokens->token->wme->slot->attr,
                                     DECLARE_SYMBOL(symbol_type_symbolic_constant,
                                                    OPERATOR_CONSTANT))
                && (tokens->token->wme->type == wme_type_normal)
                && agent_is_goal(agent, tokens->token->wme->slot->id)) {
                support = support_type_osupport;
            }
        }

        /* Propagate tokens downward. */
        rete_initialize_matches(agent, memory, parent);

        parent = node;
        ++depth;

        /* Now see if we can attach any of the `nots' that we've
           collected. We'll keep trying until we can't attach any more
           `nots'. This places the negative conditions as high as
           possible in the network, which is probably a good thing. */
        while (1) {
            struct token_list **link, *nots;

            for (link = &chunk->nots; (nots = *link) != 0; link = &nots->next) {
                if (tests_check_superstate(agent, nots->token->parent->node->data.tests,
                                           nots->token, chunk->level))
                    break;
            }

            /* If we've checked the entire list, then none of the
               `nots' could be attached here. */
            if (! nots)
                break;

            /* Otherwise, build a negative condition node, and attach
               it to the current position in the network. */
#ifdef DEBUG_CHUNKING
            printf("building negative condition from ");
            debug_dump_token(&symtab, nots->token);
            printf("\n");
#endif

            parent =
                create_negative_node(agent, chunk, &bindings,
                                     nots->token, parent, depth);

            ++depth;

            /* Remove the token from the list so we don't try to
               attach it again. */
            *link = nots->next;
            free(nots);
        }

        /* Free the token list entry. */
        doomed = tokens;
        tokens = tokens->next;
        free(doomed);
    }

    /* Make the chunk's right-hand side. */
    make_rhs(agent, parent, --depth, bindings, results, support, chunk->justification);

    /* Clean up the variable binding list. */
    while (bindings) {
        struct variable_binding_list *doomed = bindings;
        bindings = bindings->next;
        free(doomed);
    }

#ifdef DEBUG_CHUNKING
    debug_dump_beta_node(&symtab, agent->root_node->children, 0, 1);
#endif
}

/*
 * Return `true' if the `id' field of the wme for the specified token
 * is reachable from a value in the grounds set.
 */
static bool_t
token_is_reachable(struct token_list *grounds,
                   struct token      *token)
{
    for ( ; grounds != 0; grounds = grounds->next) {
        if (SYMBOLS_ARE_EQUAL(grounds->token->wme->value, token->wme->slot->id))
            return 1;
    }

    return 0;
}

/*
 * Return `true' if the token is already in the list.
 */
static bool_t
token_list_contains(struct token_list *list,
                    struct token      *token)
{
    for ( ; list != 0; list = list->next) {
        if (list->token == token)
            return 1;
    }

    return 0;
}

/*
 * Collect tokens from the specified instantiation. Add each token
 * either to the grounds or to the potentials. Move any potentials to
 * the grounds set that have become reachable from the grounds.
 */
static void
collect(struct agent          *agent,
        struct chunk          *chunk,
        struct instantiation  *inst)
{
    struct instantiation_list *visited;
    struct token_list *potential;
    struct token *token;

#ifdef DEBUG_CHUNKING
    printf("collecting tokens from %s:\n", inst->production->name);
#endif

    /* Note that we've visitied this instantiation so we don't visit
       it again. */
    visited = (struct instantiation_list *) malloc(sizeof(struct instantiation_list));
    visited->inst = inst;
    visited->next = chunk->visited;
    chunk->visited = visited;

    /* Iterate through each token in the instantiation: if it's a
       higher-level goal, then add it to the grounds. Otherwise, note
       that it is a potential that needs to be backtraced. */
    for (token = inst->token; token != 0; token = token->parent) {
        struct wme *wme = token->wme;
        struct token_list *entry, **link;

        if (token_list_contains(chunk->grounds, token))
            continue;

        if (wme) {
            if (agent_get_id_level(agent, wme->slot->id) < chunk->level
                && agent_is_goal(agent, wme->slot->id)) {
                link = &chunk->grounds;
            }
            else {
                link = &chunk->potentials;
            }
        }
        else if (token->parent
                 && token->parent->node->type == beta_node_type_negative
                 && !token_list_contains(chunk->nots, token)) {
            link = &chunk->nots;
        }
        else
            continue;

        entry = (struct token_list *) malloc(sizeof(struct token_list));
        entry->token = token;

        entry->next = *link;
        *link = entry;

#ifdef DEBUG_CHUNKING
        if (link == &chunk->grounds)
            printf("  grounds += ");
        else if (link == &chunk->potentials)
            printf("  potentials += ");
        else
            printf("  nots += ");

        debug_dump_token(&symtab, token);
        printf("\n");
#endif
    }

    /* Iteratively move any potentials that are reachable from the
       current set of grounds to the grounds set. Keep doing this
       until nothing new becomes reachable. */
    do {
        struct token_list **link = &chunk->potentials;
        for ( ; (potential = *link) != 0; link = &potential->next) {
            if (token_is_reachable(chunk->grounds, potential->token)) {
                *link = potential->next;
                potential->next = chunk->grounds;
                chunk->grounds = potential;

#ifdef DEBUG_CHUNKING
                debug_dump_token(&symtab, potential->token);
                printf(": potentials -> grounds\n");
#endif

                break;
            }
        }
    } while (potential);
}

static bool_t
instantiation_list_contains(struct instantiation_list *list,
                            struct instantiation      *inst)
{
    for ( ; list != 0; list = list->next) {
        if (list->inst == inst)
            return 1;
    }

    return 0;
}

/*
 * Backtrace through each token in the potentials set.
 */
static void
backtrace(struct agent *agent, struct chunk *chunk)
{
    struct token_list *potential;

    while ((potential = chunk->potentials) != 0) {
        struct wme *wme = potential->token->wme;
        struct preference *pref;

        chunk->potentials = potential->next;

        /* See if we've tested `^quiescence t' at the bottom-most
           level. If so, then this will be a justification, not a
           chunk. */
        if (SYMBOLS_ARE_EQUAL(wme->slot->attr,
                              DECLARE_SYMBOL(symbol_type_symbolic_constant,
                                             QUIESCENCE_CONSTANT))
            && SYMBOLS_ARE_EQUAL(wme->value,
                                 DECLARE_SYMBOL(symbol_type_symbolic_constant,
                                                T_CONSTANT))
            && (agent_get_id_level(agent, wme->slot->id) == chunk->level)) {
            chunk->justification = 1;
        }
        else {
            /* Look for a preference at the current goal level that
               supports this wme. If we find one, then we've got a new
               instantiation to collect tokens from. If not, then this
               potential is a dead end. */
            for (pref = wme->slot->preferences;
                 pref != 0;
                 pref = pref->next_in_slot) {
                if (pref->type == preference_type_acceptable
                    && SYMBOLS_ARE_EQUAL(pref->value, wme->value)
                    && pref->instantiation
                    && !instantiation_list_contains(chunk->visited,
                                                    pref->instantiation)
                    && (rete_get_instantiation_level(agent, pref->instantiation)
                        == chunk->level)) {

#ifdef DEBUG_CHUNKING
                    printf("backtracing ");
                    debug_dump_wme(&symtab, wme);
                    printf(" through ");
                    debug_dump_preference(&symtab, pref);
                    printf("\n");
#endif

                    /* This will push any new potentials to the start
                       of the queue, leading to a depth-first
                       traversal through the potential set. */
                    collect(agent, chunk, pref->instantiation);
                    break;
                }
            }
        }

        free(potential);
    }
}

/*
 * Build a chunk that creates the specified results from the
 * instantiation.
 */
static void
chunk(struct agent           *agent,
      struct instantiation   *inst,
      int                     level,
      struct preference_list *results)
{
    struct chunk chunk;

    chunk.grounds       = 0;
    chunk.potentials    = 0;
    chunk.nots          = 0;
    chunk.visited       = 0;
    chunk.level         = level;
    chunk.justification = 0;

    /* Collect the initial set of grounds for this instantiation. */
    collect(agent, &chunk, inst);

    /* Continue to backtrace potentials until the list is
       exhausted.
       XXX we need to keep track of the instantiations that we've
       visited, or else this may not terminate. */
    while (chunk.potentials)
        backtrace(agent, &chunk);

    /* Free the storage we're using to remember visited
       instantiations. */
    while (chunk.visited) {
        struct instantiation_list *doomed = chunk.visited;
        chunk.visited = chunk.visited->next;
        free(doomed);
    }

    /* Sort the grounds s.t. parents appear before their children. */
    {
        struct token_list *i, *j;
        for (i = chunk.grounds; i != 0; i = i->next) {
            for (j = i->next; j != 0; j = j->next) {
                struct token *token;
                for (token = i->token; token != 0; token = token->parent) {
                    if (token->wme && token->wme->value == j->token->wme->slot->id) {
                        token = j->token;
                        j->token = i->token;
                        i->token = token;
                        break;
                    }
                }
            }
        }
    }

#ifdef DEBUG_CHUNKING
    {
        struct token_list *tokens;
        printf("final grounds:\n");
        for (tokens = chunk.grounds; tokens != 0; tokens = tokens->next) {
            debug_dump_token(&symtab, tokens->token);
            printf("\n");
        }
    }
#endif

    /* Build the chunk from the grounds. */
    make_production(agent, &chunk, results);

    /* Free any of the `nots' that weren't used in the chunk. */
    while (chunk.nots) {
        struct token_list *doomed = chunk.nots;
        chunk.nots = chunk.nots->next;
        free(doomed);
    }
}

/*
 * Return `true' if the preference list contains the specified
 * preference.
 */
static bool_t
preference_list_contains(struct preference_list *list, struct preference *pref)
{
    for ( ; list != 0; list = list->next) {
        if (list->preference == pref)
            return 1;
    }

    return 0;
}

struct close_results_enum_data {
    struct preference_list **results;
    symbol_t                 id;
    bool_t                   changed;
};

/*
 * Hashtable enumeration callback that adds any live, acceptable,
 * preferences with the same identifier to the result set.
 */
static ht_enumerator_result_t
close_results_helper(struct ht_entry_header *entry, void *closure)
{
    struct close_results_enum_data *data =
        (struct close_results_enum_data *) closure;

    struct slot *slot = (struct slot *) HT_ENTRY_DATA(entry);

    if (SYMBOLS_ARE_EQUAL(slot->id, data->id)) {
        struct preference *pref;

        for (pref = slot->preferences; pref != 0; pref = pref->next_in_slot) {
            if ((pref->state == preference_state_live)
                && (pref->type == preference_type_acceptable)
                && !preference_list_contains(*(data->results), pref)) {
                struct preference_list *entry = (struct preference_list *)
                    malloc(sizeof(struct preference_list));

                entry->preference = pref;
                entry->next       = *(data->results);

                *(data->results) = entry;

                data->changed = 1;
            }
        }
    }

    return ht_enumerator_result_ok;
}

/*
 * Compute the transitive closure of the specified result set.
 */
static void
close_results(struct agent            *agent,
              int                      level,
              struct preference_list **results)
{
    struct close_results_enum_data data;
    data.results = results;

    do {
        struct preference_list *list;

        data.changed = 0;
        
        /* Scan the list of preferences. If an acceptable `value'
           preference is made for an identifier at the current
           (bottom-most) goal level, then add any preferences
           reachable from the value's identifier. */
        for (list = *results; list != 0; list = list->next) {
            struct preference *pref = list->preference;
            if ((pref->type == preference_type_acceptable)
                && (GET_SYMBOL_TYPE(pref->value) == symbol_type_identifier)) {
                /* The identifier's level may be zero if it's first
                   created by this instantiation. */
                int id_level = agent_get_id_level(agent, pref->value);
                if (!id_level || (agent_get_id_level(agent, pref->value) == level)) {
                    data.id = pref->value;
                    ht_enumerate(&agent->slots, close_results_helper, &data);
                }
            }
        }
    } while (data.changed);
}

/*
 * Append the specified preference to the result set if it refers to a
 * goal above the specified level.
 */
static void
append_if_result(struct agent            *agent,
                 struct preference_list **results,
                 int                      level,
                 struct preference       *pref)
{
    int id_level = agent_get_id_level(agent, pref->slot->id);
    if (id_level && id_level < level) {
        /* Include the preference as a result. */
        struct preference_list *entry =
            (struct preference_list *) malloc(sizeof(struct preference_list));

        entry->preference = pref;
        entry->next       = *results;
        *results          = entry;
    }
}

/*
 * Examine the instantiation to determine if any results were returned
 * to a higher goal level. If so, create a chunk.
 */
void
chunk_if_results(struct agent         *agent,
                 struct instantiation *inst,
                 struct preference    *o_rejects,
                 int                   level)
{
    struct preference_list *results;
    struct preference *pref;

    results = 0;
    for (pref = inst->preferences; pref != 0; pref = pref->next_in_instantiation)
        append_if_result(agent, &results, level, pref);

    for (pref = o_rejects; pref != 0; pref = pref->next_in_slot)
        append_if_result(agent, &results, level, pref);

    /* If we created results, then compute the transitive closure of
       the result set and build a chunk. */
    if (results) {
        struct preference_list *doomed;

        close_results(agent, level, &results);
        chunk(agent, inst, level, results);

        do {
            doomed = results;
            results = results->next;
            free(doomed);
        } while (results);
    }
}

