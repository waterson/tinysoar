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

struct chunk {
    struct token_list *grounds;
    struct token_list *potentials;
    int                level;
};

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

static void
copy_tests(struct beta_test             **dest,
           struct beta_test              *src,
           struct variable_binding_list **bindings,
           int                            depth,
           struct wme                    *wme)
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
                variable_binding_t *binding;
                symbol_t variable;
                int relative_depth;

                switch (test->field) {
                case field_id:    variable = wme->slot->id;   break;
                case field_attr:  variable = wme->slot->attr; break;
                case field_value: variable = wme->value;      break;
                default:
                    ERROR(("unexpected field"));
                }

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
                       bindings, depth, wme);
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

static void
make_rhs(struct agent                 *agent,
         struct beta_node             *parent,
         int                           depth,
         struct variable_binding_list *bindings,
         struct preference_list       *results,
         support_type_t                support)
{
    struct production *production;
    struct beta_node *node;
    struct symbol_list *unbound_vars;

    /* Create a production structure. */
    production = (struct production *) malloc(sizeof(struct production));
    production->support        = support_type_isupport;
    production->instantiations = 0;
    production->actions        = 0;

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
 *
 */
static void
make_production(struct agent            *agent,
                struct token_list      **grounds,
                struct preference_list  *results)
{
    struct variable_binding_list *bindings = 0;
    struct beta_node *parent = agent->root_node;
    struct token_list *tokens;
    int depth = 0;
    bool_t tested_goal = 0;
    support_type_t support = support_type_isupport;

    /* Create beta nodes for each token.

       XXX this entire swath of code is atrocious, and needs to be
       re-written once I figure out what I'm doing. */
    for (tokens = *grounds; tokens != 0; tokens = tokens->next) {
        struct beta_node *memory;
        struct beta_node *orig = tokens->token->node;
        struct beta_node *node;

        node = (struct beta_node *) malloc(sizeof(struct beta_node));

        if (! orig) {
            /* This is a dummy token created for an ^item. */

            /* Create a memory node that will be the parent. */
            memory = (struct beta_node *) malloc(sizeof(struct beta_node));
            memory->type = beta_node_type_memory;
            memory->alpha_node = 0;
            memory->tokens = 0;
            memory->parent = parent;
            memory->children = 0;
            memory->siblings = parent->children;
            parent->children = memory;

            node->type = beta_node_type_positive_join;

            {
                /* XXX find_alpha_node */
                struct alpha_node *alpha_node =
                    agent->alpha_nodes[get_alpha_test_index(0, OPERATOR_CONSTANT, 0, wme_type_acceptable)];

                for ( ; alpha_node != 0; alpha_node = alpha_node->siblings) {
                    if (GET_SYMBOL_VALUE(alpha_node->attr) == OPERATOR_CONSTANT)
                        break;
                }

                ASSERT(alpha_node != 0, ("couldn't find an alpha node"));

                node->alpha_node = alpha_node;
                node->next_with_same_alpha_node = alpha_node->children;
                alpha_node->children = node;
            }

            {
                struct beta_test *test;
                int relative_depth;

                node->data.tests = 0;

                if (get_variable_binding(bindings, tokens->token->wme->slot->id)) {
                    /* Bind the id. */
                    test = (struct beta_test *) malloc(sizeof(struct beta_test));
                    test->type = test_type_equality;
                    test->relational_type = relational_type_variable;
                    test->field = field_id;
                    test->data.variable_referent =
                        *ensure_variable_binding(&bindings, tokens->token->wme->slot->id, field_id, depth);

                    relative_depth = 
                        depth - GET_VARIABLE_BINDING_DEPTH(test->data.variable_referent);

                    SET_VARIABLE_BINDING_DEPTH(test->data.variable_referent, relative_depth);

                    test->next = node->data.tests;
                    node->data.tests = test;
                }

                if (get_variable_binding(bindings, tokens->token->wme->value)) {
                    /* Bind the value. */
                    test = (struct beta_test *) malloc(sizeof(struct beta_test));
                    test->type = test_type_equality;
                    test->relational_type = relational_type_variable;
                    test->field = field_value;
                    test->data.variable_referent =
                        *ensure_variable_binding(&bindings, tokens->token->wme->value, field_value, depth);

                    relative_depth = 
                        depth - GET_VARIABLE_BINDING_DEPTH(test->data.variable_referent);

                    SET_VARIABLE_BINDING_DEPTH(test->data.variable_referent, relative_depth);

                    test->next = node->data.tests;
                    node->data.tests = test;
                }

                if (! tested_goal) {
                    /* Make sure the id is-a goal. */
                    test = (struct beta_test *) malloc(sizeof(struct beta_test));
                    test->type = test_type_goal_id;
                    test->field = field_id;
                    test->next = node->data.tests;
                    node->data.tests = test;

                    tested_goal = 1;
                }

                ASSERT(node->data.tests != 0, ("uh, no tests!"));
            }

            node->tokens = 0;
            node->children = 0;
        }
        else if (orig->parent->type == beta_node_type_positive_join) {
            /* Create a memory node that will be the parent. */
            memory = (struct beta_node *) malloc(sizeof(struct beta_node));
            memory->type = beta_node_type_memory;
            memory->alpha_node = 0;
            memory->tokens = 0;
            memory->parent = parent;
            memory->children = 0;
            memory->siblings = parent->children;
            parent->children = memory;

            node->type = beta_node_type_positive_join;

            /* Link it into the alpha network, if appropriate. */
            node->alpha_node = orig->parent->alpha_node;
            if (node->alpha_node) {
                node->next_with_same_alpha_node = node->alpha_node->children;
                node->alpha_node->children = node;
            }
            else
                node->next_with_same_alpha_node = 0;

            /* Copy the tests. */
            ensure_variable_binding(&bindings, tokens->token->wme->slot->id, field_id, depth);

            if (GET_SYMBOL_TYPE(tokens->token->wme->value) == symbol_type_identifier)
                ensure_variable_binding(&bindings, tokens->token->wme->value, field_value, depth);

            /* Compute o-support. */
            if ((support == support_type_isupport)
                && SYMBOLS_ARE_EQUAL(tokens->token->wme->slot->attr, OPERATOR_CONSTANT)
                && (tokens->token->wme->type == wme_type_normal)
                && agent_is_goal(agent, tokens->token->wme->slot->id)) {
                support = support_type_osupport;
            }

            node->data.tests = 0;
            copy_tests(&node->data.tests, orig->parent->data.tests, &bindings,
                       depth, tokens->token->wme);

            if (! tested_goal && agent_is_goal(agent, tokens->token->wme->slot->id)) {
                /* Make sure the id is-a goal. */
                struct beta_test *test;

                test = (struct beta_test *) malloc(sizeof(struct beta_test));
                test->type = test_type_goal_id;
                test->field = field_id;
                test->next = node->data.tests;
                node->data.tests = test;

                tested_goal = 1;
            }

            node->tokens = 0;
            node->children = 0;
        }
        else {
            UNIMPLEMENTED();
        }

        /* Link it into the beta tree. */
        node->parent = memory;
        node->siblings = memory->children;
        memory->children = node;

        /* Propagate tokens downward. */
        rete_initialize_matches(agent, memory, parent);

        parent = node;

        ++depth;
    }

    /* Make the chunk's right-hand side. */
    make_rhs(agent, parent, --depth, bindings, results, support);

    /* Clean up the variable binding list. */
    while (bindings) {
        struct variable_binding_list *doomed = bindings;
        bindings = bindings->next;
        free(doomed);
    }
}

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

static void
collect(struct agent          *agent,
        struct chunk          *chunk,
        struct instantiation  *inst)
{
    struct token_list *potential;
    struct token *token;

    /* Iterate through each token in the instantiation: if it's a
       higher-level goal, then add it to the grounds. Otherwise, note
       that it is a potential that needs to be backtraced. */
    for (token = inst->token; token != 0; token = token->parent) {
        struct wme *wme = token->wme;
        if (wme) {
            struct token_list *entry;

            if (token_list_contains(chunk->grounds, token))
                continue;

            entry = (struct token_list *) malloc(sizeof(struct token_list));
            entry->token = token;

            if (agent_get_id_level(agent, wme->slot->id) < chunk->level
                && agent_is_goal(agent, wme->slot->id)) {
                /* Token goes in the grounds. */
                entry->next = chunk->grounds;
                chunk->grounds = entry;
            }
            else {
                /* Token needs to be backtraced. */
                entry->next = chunk->potentials;
                chunk->potentials = entry;
            }
        }
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
                break;
            }
        }
    } while (potential);
}

static void
backtrace(struct agent *agent, struct chunk *chunk)
{
    struct token_list *potential;

    while ((potential = chunk->potentials) != 0) {
        struct wme *wme;
        struct preference *pref;

        chunk->potentials = potential->next;

        /* Look for a preference at the current goal level that
           supports this wme. */
        wme = potential->token->wme;
        for (pref = wme->slot->preferences;
             pref != 0;
             pref = pref->next_in_slot) {
            if (pref->type == preference_type_acceptable
                && SYMBOLS_ARE_EQUAL(pref->value, wme->value)
                && pref->instantiation
                && rete_get_instantiation_level(agent, pref->instantiation)) {
                break;
            }
        }

        /* XXX may never terminate because we don't keep trace of
           which instantiations we've visited. */
        if (pref)
            collect(agent, chunk, pref->instantiation);
        else
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

    chunk.grounds    = 0;
    chunk.potentials = 0;
    chunk.level      = level;

    /* Collect the initial set of grounds for this instantiation. */
    collect(agent, &chunk, inst);

    /* Continue to backtrace potentials until the list is
       exhausted.
       XXX we need to keep track of the instantiations that we've
       visited, or else this may not terminate. */
    while (chunk.potentials)
        backtrace(agent, &chunk);

    /* Sort the grounds s.t. parents appear before their children. */
    {
        struct token_list *i, *j;
        for (i = chunk.grounds; i != 0; i = i->next) {
            for (j = i->next; j != 0; j = j->next) {
                struct token *token;
                for (token = i->token; token != 0; token = token->parent) {
                    if (token->wme->value == j->token->wme->slot->id) {
                        token = j->token;
                        j->token = i->token;
                        i->token = token;
                        break;
                    }
                }
            }
        }
    }

#ifdef DEBUG
    {
        extern void dump_token(struct symtab *, struct token *);
        extern struct symtab symtab;
        struct token_list *tokens;
        for (tokens = chunk.grounds; tokens != 0; tokens = tokens->next) {
            dump_token(&symtab, tokens->token);
            printf("\n");
        }
    }
#endif

    /* Build the chunk from the grounds. */
    make_production(agent, &chunk.grounds, results);

    /* Free memory we've used. */
    while (chunk.grounds) {
        struct token_list *doomed = chunk.grounds;
        chunk.grounds = chunk.grounds->next;
        free(doomed);
    }
}

/*
 * Compute the transitive closure of the specified result set.
 */
static void
close_results(struct agent            *agent,
              int                      level,
              struct preference_list **results)
{
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

