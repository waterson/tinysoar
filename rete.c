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
 * Runtime support for the RETE network that filters the addition
 * and removal of working memory elements through the RETE network.
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

#if defined(HAVE_PRINTF) && defined(DEBUG)
#  include <stdio.h>
#  include "symtab.h"
#endif

/*
 * Create a new token
 */
static inline struct token *
create_token(struct beta_node *node,
             struct token     *parent,
             struct wme       *wme)
{
    struct token *result = (struct token *) malloc(sizeof(struct token));
    result->parent = parent;
    result->node   = node;
    result->wme    = wme;
    return result;
}

/*
 * Select a field from the specified wme.
 */
static symbol_t
get_field_from_wme(struct wme *wme, field_t field)
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
 * Add a wme to the specified alpha node's right memory
 */
/*static inline*/ void
add_wme_to_alpha_node(struct agent      *agent,
                      struct alpha_node *node,
                      struct wme        *wme)
{
    struct right_memory *rm =
        (struct right_memory *) malloc(sizeof(struct right_memory));

    rm->wme = wme;
    rm->next_in_alpha_node = node->right_memories;
    node->right_memories = rm;
}

/*
 * Remove a wme from the specified alpha node's right memory
 */
static inline void
remove_wme_from_alpha_node(struct agent      *agent,
                           struct alpha_node *node,
                           struct wme        *wme)
{
    struct right_memory *rm, **link;
    for (link = &node->right_memories;
         (rm = *link) != 0;
         link = &rm->next_in_alpha_node) {
        if (rm->wme == wme) {
            *link = rm->next_in_alpha_node;
            free(rm);
            break;
        }
    }
}

/*
 * Check a single beta test
 */
static bool_t
check_beta_test(struct agent     *agent,
                struct beta_test *test,
                struct token     *token,
                struct wme       *wme)
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
                struct wme *left_wme = wme;
                int depth = (int) GET_VARIABLE_BINDING_DEPTH(test->data.variable_referent);
                if (depth) {
                    /* Start with |depth == 1| referring to the WME
                       from the token. */
                    struct token *t = token;

                    /* Walk up the token for anything deeper. */
                    while (--depth >= 1)
                        t = t->parent;

                    left_wme = t->wme;
                }

                left = get_field_from_wme(left_wme, GET_VARIABLE_BINDING_FIELD(test->data.variable_referent));
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
            struct symbol_list *goal;
            for (goal = agent->goals; goal != 0; goal = goal->next) {
                if (SYMBOLS_ARE_EQUAL(goal->symbol, wme->slot->id))
                    return 1;
            }
        }
        break;

    case test_type_disjunctive:
        {
            struct beta_test *disjunct;
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
check_beta_tests(struct agent     *agent,
                 struct beta_test *test,
                 struct token     *token,
                 struct wme       *wme)
{
    for ( ; test != 0; test = test->next) {
        if (! check_beta_test(agent, test, token, wme))
            return 0;
    }

    /* If we get here, all the tests passed. */
    return 1;
}

/* ---------------------------------------------------------------------- */

/*
 * Notify the beta node |node| that a new <token, wme> pair is being
 * propagated downward from its parent beta node. The working memory
 * element |wme| may extend |token| if the parent node has no storage
 * of its own.
 */
/*static*/ void
do_left_addition(struct agent     *agent,
                 struct beta_node *node,
                 struct token     *token,
                 struct wme       *wme)
{
    switch (node->type) {
    case beta_node_type_memory:
        {
            /* Add a new token to the memory and notify children */
            struct token *new_token;
            struct beta_node *child;

            new_token = create_token(node, token, wme);
            new_token->next = node->tokens;
            node->tokens = new_token;

            for (child = node->children; child != 0; child = child->siblings)
                do_left_addition(agent, child, new_token, 0);
        }
        break;

    case beta_node_type_positive_join:
        {
            struct right_memory *rm;
            for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                    struct beta_node *child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_addition(agent, child, token, rm->wme);
                }
            }
        }
        break;

    case beta_node_type_negative:
        {
            struct token *new_token;
            struct right_memory *rm;

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
                struct beta_node *child;

                new_token->next = node->tokens;
                node->tokens = new_token;

                for (child = node->children; child != 0; child = child->siblings)
                    do_left_addition(agent, child, new_token, 0);
            }
        }
        break;

    case beta_node_type_production:
        {
            struct token *new_token = create_token(node, token, wme);
            struct match *match;

            new_token->next = node->tokens;
            node->tokens = new_token;

            /* XXX Soar8 checks the retraction queue to see if the
               match has been retracted, and if so, removes the match
               from the retraction queue. We've got a simpler ownership
               model for tokens, so it's not possible to do that. Is
               this gonna be a problem? */

            /* Allocate a new match and place on the firing queue */
            match = (struct match *) malloc(sizeof(struct match));
            match->data.token = new_token;
            match->production = node->data.production;
            match->next       = agent->assertions;
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

/*
 * Notify the beta node |node| that the <token, wme> pair are about to
 * be removed from the parent beta node.
 */
static void
do_left_removal(struct agent     *agent,
                struct beta_node *node,
                struct token     *token,
                struct wme       *wme)
{
    switch (node->type) {
    case beta_node_type_memory:
        {
            struct token *doomed, **link;
            for (link = &node->tokens; (doomed = *link) != 0; link = &doomed->next) {
                if ((doomed->wme == wme) && (doomed->parent == token)) {
                    struct beta_node *child;
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
            struct right_memory *rm;
            for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                if (check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                    struct beta_node *child;
                    for (child = node->children; child != 0; child = child->siblings)
                        do_left_removal(agent, child, token, rm->wme);
                }
            }
        }
        break;

    case beta_node_type_negative:
        {
            struct token *doomed, **link;

            /* First see if this was a ``blocked'' token, in which
               case its removal will have no side effects */
            for (link = &node->blocked; (doomed = *link) != 0; link = &doomed->next) {
                if ((doomed->wme == wme) && (doomed->parent == token)) {
                    *link = doomed->next;
                    free(doomed);
                    break;
                }
            }

            if (! doomed) {
                /* Okay, it must've been one of the tokens that we'd propagated
                   downwards. Find it and yank it. */
                for (link = &node->tokens; (doomed = *link) != 0; link = &doomed->next) {
                    if ((doomed->wme == wme) && (doomed->parent == token)) {
                        struct beta_node *child;
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
            struct match *match;

            /* See if this match is new */
            {
                struct match **link;

                for (link = &agent->assertions; (match = *link) != 0; link = &match->next) {
                    if ((match->data.token->wme == wme) && (match->data.token->parent == token)) {
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
                struct instantiation *inst;
                for (inst = node->data.production->instantiations; inst != 0; inst = inst->next) {
                    if ((inst->token->wme == wme) && (inst->token->parent == token)) {
                        /* See if this match is already on the retraction queue */
                        for (match = agent->retractions; match != 0; match = match->next) {
                            if (match->data.instantiation == inst)
                                break;
                        }

                        if (! match) {
                            /* Gotcha. Allocate a new match and place on the
                               retraction queue */
                            match = (struct match *) malloc(sizeof(struct match));
                            match->data.instantiation = inst;
                            match->production         = node->data.production;
                            match->next               = agent->retractions;

                            agent->retractions = match;
                        }
                    }
                }
            }

            /* Nuke the token */
            {
                struct token *doomed, **link;
                for (link = &node->tokens; (doomed = *link) != 0; link = &doomed->next) {
                    if ((doomed->wme == wme) && (doomed->parent == token)) {
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
/*static inline*/ void
do_right_addition(struct agent *agent, struct beta_node *node, struct wme *wme)
{
    struct token **link, *token = node->tokens;

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
                struct beta_node *child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_addition(agent, child, token, wme);
            }

            token = token->next;
        }
        break;

    case beta_node_type_negative:
        /* Iterate through the ``active'' tokens to see if any will be
           blocked by this addition */
        link = &node->tokens;
        while ((token = *link) != 0) {
            if (!node->data.tests ||
                check_beta_tests(agent, node->data.tests, token, wme)) {
                /* If there are no beta tests, or the beta tests
                   all pass, then the negative condition has
                   matched. We need to remove any tokens that had
                   previously been propagated. */
                struct beta_node *child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_removal(agent, child, token, 0);

                /* This token is now ``blocked'' */
                *link = token->next;

                token->next = node->blocked;
                node->blocked = token;
            }
            else {
                /* This token remains unblocked: on to the next one. */
                link = &token->next;
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

/*
 * Notify the beta node |node| that |wme| is about to be removed from
 * the right-memory to which |node| is attached.
 */
static inline void
do_right_removal(struct agent *agent, struct beta_node *node, struct wme *wme)
{
    struct token *token = node->tokens, **link;

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
                struct beta_node *child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_removal(agent, child, token, wme);
            }

            token = token->next;
        }
        break;

    case beta_node_type_negative:
        /* Iterate through the blocked tokens to see if any will be
           unblocked by the right-memory removal. */
        link = &node->blocked;
        while ((token = *link) != 0) {
            /* Check the remaining right memories (we'll skip the one
               that we're removing) to see if the token is still
               blocked. */
            struct right_memory *rm;
            for (rm = node->alpha_node->right_memories;
                 rm != 0;
                 rm = rm->next_in_alpha_node) {
                if (rm->wme == wme)
                    continue;

                /* If there are no beta tests, or the beta tests all
                   pass, then this token is still blocked. */
                if (!node->data.tests ||
                    check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                    break;
                }
            }

            if (rm) {
                /* Still blocked: move on to the next blocked token. */
                link = &token->next;
            }
            else {
                /* We didn't find any right-memories that block the
                   token. Propagate it downwards. */
                struct beta_node *child;
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

#ifdef CONF_SOAR_RETE_CREATE
void
rete_create(struct agent *agent)
{
    int i;

    struct beta_node *root_node =
        (struct beta_node *) malloc(sizeof(struct beta_node));

    root_node->type = beta_node_type_root;
    root_node->parent
        = root_node->siblings
        = root_node->next_with_same_alpha_node
        = root_node->children = 0;

    root_node->alpha_node = 0;

    root_node->tokens = &agent->root_token;

    agent->root_node = root_node;

    agent->root_token.parent = 0;
    agent->root_token.node   = agent->root_node;
    agent->root_token.wme    = 0;
    agent->root_token.next   = 0;

#define NUM_ALPHA_NODES (sizeof(agent->alpha_nodes) / sizeof(struct alpha_node *))
    for (i = NUM_ALPHA_NODES - 1; i >= 0; --i)
        agent->alpha_nodes[i] = 0;

    agent->assertions = agent->retractions = 0;
}
#endif

void
rete_init(struct agent *agent)
{
    struct beta_node *node = agent->root_node->children;
    while (node) {
        do_left_addition(agent, node, &agent->root_token, 0);
        node = node->siblings;
    }
}

void
rete_operate_wme(struct agent *agent, struct wme *wme, wme_operation_t op)
{
    int offset = (wme->type == wme_type_normal) ? 0 : 8;
    int i;

#if 0 /* Use to debug wme additions and removals. */
    extern struct symtab symtab;
    static void dump_wme(struct symtab *symtab, struct wme *wme);
    printf("%c ", (op == wme_operation_add ? '+' : '-'));
    dump_wme(&symtab, wme);
    printf("\n");
#endif

    for (i = 0; i < 8; ++i) {
        struct alpha_node *alpha;

        for (alpha = agent->alpha_nodes[i + offset]; alpha != 0; alpha = alpha->siblings) {
            if (wme_matches_alpha_node(wme, alpha)) {
                if (op == wme_operation_add) {
                    struct beta_node *beta;
                    add_wme_to_alpha_node(agent, alpha, wme);

                    for (beta = alpha->children; beta != 0; beta = beta->next_with_same_alpha_node)
                        do_right_addition(agent, beta, wme);
                }
                else {
                    struct beta_node *beta;
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
rete_get_variable_binding(variable_binding_t binding, struct token *token)
{
    int depth = (int) GET_VARIABLE_BINDING_DEPTH(binding);
    while (--depth >= 0)
        token = token->parent;

    return get_field_from_wme(token->wme, GET_VARIABLE_BINDING_FIELD(binding));
}


void
rete_finish(struct agent *agent)
{
    UNIMPLEMENTED();
}

#if defined(HAVE_PRINTF) && defined(DEBUG)

static void
indent_by(int nest)
{
    int i;
    for (i = 0; i < nest; ++i)
        printf("  ");
}

static const char *
symbol_to_string(struct symtab *symtab, symbol_t symbol)
{
    static char buf[16];

    if (SYMBOL_IS_NIL(symbol))
        return "*";

    switch (GET_SYMBOL_TYPE(symbol)) {
    case symbol_type_symbolic_constant:
        if (symtab)
            return symtab_find_name(symtab, symbol);

        sprintf(buf, "(%d)", GET_SYMBOL_VALUE(symbol));
        break;

    case symbol_type_identifier:
        sprintf(buf, "[%d]", GET_SYMBOL_VALUE(symbol));
        break;

    case symbol_type_integer_constant:
        sprintf(buf, "%d", GET_SYMBOL_VALUE(symbol));
        break;

    case symbol_type_variable:
        sprintf(buf, "<%d>", GET_SYMBOL_VALUE(symbol));
        break;
    }

    return buf;
}

static void
dump_wme(struct symtab *symtab, struct wme *wme)
{
    printf("wme@%p(%s ", wme, symbol_to_string(symtab, wme->slot->id));
    printf("^%s ", symbol_to_string(symtab, wme->slot->attr));
    printf("%s", symbol_to_string(symtab, wme->value));
    if (wme->type == wme_type_acceptable)
        printf(" +");
    printf(")");
}

static void
dump_token(struct symtab *symtab, struct token *token)
{
    printf("token@%p<parent=%p wme=", token, token->parent);
    if (token->wme)
        dump_wme(symtab, token->wme);
    else
        printf("(null)");
    printf(">");
}

static void
dump_variable_binding(variable_binding_t binding)
{
    printf("<%d,", GET_VARIABLE_BINDING_DEPTH(binding));
    switch (GET_VARIABLE_BINDING_FIELD(binding)) {
    case field_id:    printf("id");     break;
    case field_attr:  printf("attr");   break;
    case field_value: printf("value");  break;
    default:
        ERROR(("unexpected field"));
    }
    printf(">");
}

static void
dump_test(struct symtab *symtab, struct beta_test *test)
{
    switch (test->field) {
    case field_id:      printf("id");     break;
    case field_attr:    printf("attr");   break;
    case field_value:   printf("value");  break;
    default:
        ERROR(("unexpected field"));
    }

    switch (test->type) {
    case test_type_blank:
        printf("0");
        break;

    case test_type_equality:
        printf("==");
        break;

    case test_type_not_equal:
        printf("!=");
        break;

    case test_type_less:
        printf("<");
        break;

    case test_type_greater:
        printf(">");
        break;

    case test_type_less_or_equal:
        printf("<=");
        break;

    case test_type_greater_or_equal:
        printf(">=");
        break;

    case test_type_same_type:
        printf("<=>");
        break;

    case test_type_disjunctive:
        {
            struct beta_test *disjunct = test->data.disjuncts;
            printf("(");
            while (disjunct) {
                dump_test(symtab, disjunct);

                disjunct = disjunct->next;

                if (disjunct)
                    printf(" || ");
            }
            printf(")");
        }
        return;

    case test_type_conjunctive:
        /* shouldn't ever hit this; conjunctive tests are
           converted into a list of single tests. */
        ERROR(("unexpected test"));
        break;

    case test_type_goal_id:
        printf("(G)");
        return;
    }

    if (test->relational_type == relational_type_constant) {
        printf("%s", symbol_to_string(symtab, test->data.constant_referent));
    }
    else {
        dump_variable_binding(test->data.variable_referent);
    }
}

static const char *
preference_type_to_string(preference_type_t type)
{
    switch (type) {
    case preference_type_acceptable:         return "+";
    case preference_type_reject:             return "-";
    case preference_type_reconsider:         return "@";
    case preference_type_unary_indifferent:  return "=";
    case preference_type_best:               return ">";
    case preference_type_worst:              return "<";
    case preference_type_prohibit:           return "~";
    case preference_type_require:            return "!";
    case preference_type_binary_indifferent: return "=";
    case preference_type_better:             return ">";
    case preference_type_worse:              return "<";
    default:
        break;
    }

    UNREACHABLE();
    return 0;
}

static void
dump_rhs_value(struct symtab *symtab, struct rhs_value *value)
{
    switch (value->type) {
    case rhs_value_type_symbol:
        printf("%s", symbol_to_string(symtab, value->val.symbol));
        break;

    case rhs_value_type_variable_binding:
        dump_variable_binding(value->val.variable_binding);
        break;

    case rhs_value_type_unbound_variable:
        printf("?%d", value->val.unbound_variable);
        break;

    default:
        UNREACHABLE();
    }
}

static void
dump_beta_node(struct symtab *symtab, struct beta_node *node, int nest, int recur)
{
    indent_by(nest);

    printf("beta-node@%p", node);

    switch (node->type) {
    case beta_node_type_memory:
        printf("(m)");
        break;

    case beta_node_type_positive_join:
        printf("(pos)");
        break;

    case beta_node_type_memory_positive_join:
        printf("(mpos)");
        break;

    case beta_node_type_negative:
        printf("(neg)");
        break;

    case beta_node_type_root:
        printf("(root)");
        break;

    case beta_node_type_conjunctive_negative:
        printf("(cneg)");
        break;

    case beta_node_type_conjunctive_negative_partner:
        printf("(cnp)");
        break;

    case beta_node_type_production:
        printf("(prod)");
        break;
    }

    printf("<parent=%p", node->parent);

    switch (node->type) {
    case beta_node_type_positive_join:
    case beta_node_type_memory_positive_join:
    case beta_node_type_negative:
        {
            struct beta_test *test;
            printf(" tests={");
            for (test = node->data.tests; test != 0; test = test->next) {
                printf(" ");
                dump_test(symtab, test);
            }
            printf(" }");
        }
        break;

    case beta_node_type_production:
        printf(" name=%s", node->data.production->name);
        break;

    default:
        break;
    }

    printf(">\n");

    if (node->type == beta_node_type_production) {
        /* Dump the RHS actions. */
        struct action *action = node->data.production->actions;
        for ( ; action != 0; action = action->next) {
            indent_by(nest + 2);
            printf("=> (");
            dump_rhs_value(symtab, &action->id);
            printf(" ^");
            dump_rhs_value(symtab, &action->attr);
            printf(" ");
            dump_rhs_value(symtab, &action->value);
            printf(" %s", preference_type_to_string(action->preference_type));
            if (action->preference_type & preference_type_binary) {
                printf(" ");
                dump_rhs_value(symtab, &action->referent);
            }
            printf(")\n");
        }
    }

    if (recur) {
        switch (node->type) {
        case beta_node_type_root:
        case beta_node_type_memory:
        case beta_node_type_memory_positive_join:
        case beta_node_type_negative:
        case beta_node_type_production:
            {
                /* dump tokens at the node */
                struct token *token;
                for (token = node->tokens; token != 0; token = token->next) {
                    indent_by(nest + 2);
                    printf("+ ");
                    dump_token(symtab, token);
                    printf("\n");
                }
            }

        default:
            break;
        }

        switch (node->type) {
        case beta_node_type_negative:
            {
                /* dump blocked tokens at the node */
                struct token *token;
                for (token = node->blocked; token != 0; token = token->next) {
                    indent_by(nest + 2);
                    printf("x ");
                    dump_token(symtab, token);
                    printf("\n");
                }
            }

        default:
            break;
        }

        for (node = node->children; node != 0; node = node->siblings)
            dump_beta_node(symtab, node, nest + 1, 1);
    }
}

void
rete_dump(struct agent *agent, struct symtab *symtab)
{
    int i;

    printf("\nALPHA NETWORK\n");
    for (i = 0; i < 16; ++i) {
        struct alpha_node *alpha = agent->alpha_nodes[i];
        while (alpha) {
            struct beta_node *beta;
            struct right_memory *rm;

            printf("(%s ", symbol_to_string(symtab, alpha->id));
            printf("^%s ", symbol_to_string(symtab, alpha->attr));
            printf("%s", symbol_to_string(symtab, alpha->value));
            if (i >= 8)
                printf(" +");
            printf(")\n");

            for (rm = alpha->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                indent_by(2);
                printf("+ ");
                dump_wme(symtab, rm->wme);
                printf("\n");
            }

            for (beta = alpha->children; beta != 0; beta = beta->next_with_same_alpha_node)
                dump_beta_node(symtab, beta, 1, 0);

            alpha = alpha->siblings;
        }
    }

    printf("\nBETA NETWORK\n");
    dump_beta_node(symtab, agent->root_node, 0, 1);
    printf("\n");
}
#endif

