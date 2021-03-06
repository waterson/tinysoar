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
    result->shared = 0;

#ifdef DEBUG_TOKEN
    printf("create_token: ");
    debug_dump_token(&symtab, result);
    printf("\n");
#endif

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
    switch (GET_BETA_TEST_TYPE(test)) {
    case test_type_equality:
    case test_type_not_equal:
    case test_type_less:
    case test_type_greater:
    case test_type_less_or_equal:
    case test_type_greater_or_equal:
    case test_type_same_type:
        {
            /* It's a relational test */
            symbol_t right = get_field_from_wme(wme, GET_BETA_TEST_FIELD(test));
            symbol_t left;

            if (GET_BETA_TEST_RELATIONAL_TYPE(test) == relational_type_constant) {
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

            switch (GET_BETA_TEST_TYPE(test)) {
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
                if (GET_BETA_TEST_TYPE(test) == test_type_same_type)
                    return 1;

                /* If we're doing relational tests, then they'll
                   only work on integer constants */
                if (GET_SYMBOL_TYPE(left) == symbol_type_integer_constant) {
                    int result = GET_SYMBOL_VALUE(right) - GET_SYMBOL_VALUE(left);
                    switch (GET_BETA_TEST_TYPE(test)) {
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
        return agent_is_goal(agent, wme->slot->id);

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
static void
do_left_addition(struct agent     *agent,
                 struct beta_node *node,
                 struct token     *token,
                 struct wme       *wme)
{
    switch (node->type) {
    case beta_node_type_memory: {
        /* Add a new token to the memory and notify children */
        struct token *new_token;
        struct beta_node *child;

        new_token = create_token(node, token, wme);
        new_token->next = node->tokens;
        node->tokens = new_token;

        for (child = node->children; child != 0; child = child->siblings)
            do_left_addition(agent, child, new_token, 0);

        break;
    }

    case beta_node_type_positive_join: {
        struct right_memory *rm;
        for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
            if (check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                struct beta_node *child;
                for (child = node->children; child != 0; child = child->siblings)
                    do_left_addition(agent, child, token, rm->wme);
            }
        }
        break;
    }

    case beta_node_type_negative: {
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

        break;
    }

    case beta_node_type_production: {
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

        break;
    }

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
static bool_t
do_left_removal(struct agent     *agent,
                struct beta_node *node,
                struct token     *token,
                struct wme       *wme)
{
    bool_t token_saved = token->shared;

    switch (node->type) {
    case beta_node_type_memory: {
        /* Iterate through the tokens we own, looking for the one that
           corresponds to the parent token and wme that are being
           removed. When we find it, pass the left-removal on to our
           children. */
        struct token *doomed, **link;
        for (link = &node->tokens; (doomed = *link) != 0; link = &doomed->next) {
            if ((doomed->wme == wme) && (doomed->parent == token)) {
                struct beta_node *child;
                for (child = node->children; child != 0; child = child->siblings) {
                    if (do_left_removal(agent, child, doomed, 0))
                        token_saved = 1;
                }

                /* Unlink the token from the list of tokens owned by
                   the node, but only free it if it's not still being
                   held by its child. */
                *link = doomed->next;

                if (token_saved) {
#ifdef DEBUG_TOKEN
                    printf("share_token: ");
                    debug_dump_token(&symtab, token);
                    printf("\n");
#endif
                    token->shared = 1;
                    doomed->next = 0;
                }
                else {
#ifdef DEBUG_TOKEN
                    printf("destroy_token: ");
                    debug_dump_token(&symtab, doomed);
                    printf("\n");
#endif

                    free(doomed);
                }

                break;
            }
        }

        /* XXX why is this okay? */
        /*ASSERT(doomed != 0, ("couldn't find token for left-removal"));*/
        break;
    }

    case beta_node_type_positive_join: {
        /* Iterate throught the join node's right memories; remove any
           wme that matches our beta tests. */
        struct right_memory *rm;
        for (rm = node->alpha_node->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
            if (check_beta_tests(agent, node->data.tests, token, rm->wme)) {
                struct beta_node *child;
                for (child = node->children; child != 0; child = child->siblings) {
                    if (do_left_removal(agent, child, token, rm->wme))
                        token_saved = 1;
                }
            }
        }
        break;
    }

    case beta_node_type_negative: {
        struct token *doomed, **link;

        /* First see if this was a `blocked' token, in which case its
           removal will have no side effects */
        for (link = &node->blocked; (doomed = *link) != 0; link = &doomed->next) {
            if ((doomed->wme == wme) && (doomed->parent == token)) {
                *link = doomed->next;

                if (! token_saved) {
#ifdef DEBUG_TOKEN
                    printf("destroy_token: ");
                    debug_dump_token(&symtab, doomed);
                    printf("\n");
#endif

                    free(doomed);
                }

                break;
            }
        }

        if (! doomed) {
            /* Okay, it must've been one of the tokens that we'd propagated
               downwards. Find it and yank it. */
            for (link = &node->tokens; (doomed = *link) != 0; link = &doomed->next) {
                if ((doomed->wme == wme) && (doomed->parent == token)) {
                    struct beta_node *child;
                    for (child = node->children; child != 0; child = child->siblings) {
                        if (do_left_removal(agent, child, doomed, 0))
                            token_saved = 1;
                    }

                    /* Unlink the token from the list of tokens owned
                       by the node, but only free it if it's not still
                       being held by its child. */
                    *link = doomed->next;

                    if (token_saved) {
#ifdef DEBUG_TOKEN
                        printf("share_token: ");
                        debug_dump_token(&symtab, token);
                        printf("\n");
#endif
                        doomed->next = 0;
                        token->shared = 1;
                    }
                    else {
#ifdef DEBUG_TOKEN
                        printf("destroy_token: ");
                        debug_dump_token(&symtab, doomed);
                        printf("\n");
#endif

                        free(doomed);
                    }

                    break;
                }
            }
        }

        ASSERT(doomed, ("couldn't find wme in left removal"));
        break;
    }

    case beta_node_type_production: {
        struct match *match;

        /* See if this match is new */
        {
            struct match **link;

            for (link = &agent->assertions; (match = *link) != 0; link = &match->next) {
                if ((match->data.token->wme == wme) && (match->data.token->parent == token)) {
                    /* Yep. Remove from the assertion queue. Since
                       this production never even got instantiated,
                       leave `token_saved' as false so that we can
                       free any tokens as we unwind. */
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
                if (inst->token
                    && (inst->token->wme == wme)
                    && (inst->token->parent == token)) {
                    /* See if this match is already on the retraction queue. */
                    /* XXX Couldn't we just remove that instantiation
                       from the production immediately so we don't
                       need to check for existence on the retraction
                       queue? (Would also eliminate `inst->token'
                       test, above.) */
                    for (match = agent->retractions; match != 0; match = match->next) {
                        if (match->data.instantiation == inst)
                            break;
                    }

                    if (! match) {
                        /* Gotcha. Allocate a new match and place on the
                           retraction queue. */

#ifdef CONF_SOAR_CHUNKING
                        int level;
#endif

                        match = (struct match *) malloc(sizeof(struct match));
                        match->data.instantiation = inst;
                        match->production         = node->data.production;
                        match->next               = agent->retractions;

#ifdef CONF_SOAR_CHUNKING
                        /* Save the token (so we can backtrace) if the
                           instantiation is from a substate. If we
                           don't save the token, then null out the
                           instantiation's pointer so we don't try to
                           free it later.

                           XXX This unilaterally does all-goals
                           chunking. We probably ought to have a
                           (compile-time?)  option to look at the goal
                           stack and do bottom-up chunking. */
                        level = rete_get_instantiation_level(agent, inst);
                        if (level > 1 && level <= agent->bottom_level)
                            token_saved = 1;
                        else {
                            ASSERT(!token_saved, ("leaking a token"));
                            inst->token = 0;
                        }
#endif

                        agent->retractions = match;
                    }

                    break;
                }
            }
        }

        /* Unlink the token from the list of tokens owned by the node,
           but only free it if it's not still being held by the
           instantiation. */
        {
            struct token *doomed, **link;
            for (link = &node->tokens; (doomed = *link) != 0; link = &doomed->next) {
                if ((doomed->wme == wme) && (doomed->parent == token)) {
                    *link = doomed->next;

                    if (token_saved) {
#ifdef DEBUG_TOKEN
                        printf("share_token: ");
                        debug_dump_token(&symtab, token);
                        printf("\n");
#endif
                        token->shared = 1;
                        doomed->next = 0;
                    }
                    else {
#ifdef DEBUG_TOKEN
                        printf("destroy_token: ");
                        debug_dump_token(&symtab, doomed);
                        printf("\n");
#endif

                        free(doomed);
                    }

                    break;
                }
            }

            ASSERT(token, ("couldn't find token in left-removal"));
        }
        break;
    }

    case beta_node_type_memory_positive_join:
    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
        UNIMPLEMENTED(); /* XXX write me! */
        break;

    case beta_node_type_root:
        ERROR(("unexpected left addition")); /* can't get left addition on this node */
        break;
    }

    return token_saved;
}

/*
 * Notify the beta node |node| that a new wme |wme| has been added to
 * the right-memory to which |node| is attached.
 */
static void
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

/*
 * Propagate matches from the parent node to its child.
 *
 * This corresponds to update_node_with_matches_from_above() from
 * rete.c in Soar8.
 */
void
rete_initialize_matches(struct agent     *agent,
                        struct beta_node *child,
                        struct beta_node *parent)
{
    switch (parent->type) {
    case beta_node_type_root:
        do_left_addition(agent, child, &agent->root_token, 0);
        break;

    case beta_node_type_positive_join: {
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
        break;
    }

    case beta_node_type_negative: {
        struct token *token;
        for (token = parent->tokens; token != 0; token = token->next)
            do_left_addition(agent, child, token, 0);

        break;
    }

    case beta_node_type_production:
    case beta_node_type_memory:
        /* We shouldn't be directly initializing matches on these
           sorts of nodes. */
        UNREACHABLE();
        break;

    case beta_node_type_memory_positive_join:
    case beta_node_type_conjunctive_negative:
    case beta_node_type_conjunctive_negative_partner:
        /* These node types don't exist yet. */
        UNIMPLEMENTED();
        break;
    }
}

/*
 * Compute the instantiation's `level'; i.e., the lowest goal level at
 * which a token matched.
 */
int
rete_get_instantiation_level(struct agent *agent, struct instantiation *inst)
{
    struct token *token;
    int level;

    level = 0;
    for (token = inst->token; token != 0; token = token->parent) {
        if (token->wme) {
            /* XXX we'll probably need to store the level in the token
               to avoid confusion when an identifier promotion occurs
               without resolving the impasse. */
            int id_level = agent_get_id_level(agent, token->wme->slot->id);

            ASSERT(id_level != 0, ("identifier without assigned level"));

            if (id_level > level)
                level = id_level;
        }
    }

    return level;
}

/*
 * Find an existing alpha node that is appropriate for testing the
 * specified fields.
 *
 * Corresponds to find_alpha_mem() in rete.c from Soar8.
 */
struct alpha_node *
rete_find_alpha_node(struct agent *agent,
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
 * Add existing WMEs to the alpha node when a new alpha node is
 * created.
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
struct alpha_node *
rete_ensure_alpha_node(struct agent *agent,
                       symbol_t      id,
                       symbol_t      attr,
                       symbol_t      value,
                       wme_type_t    type)
{
    struct alpha_node *result;

    if (! (result = rete_find_alpha_node(agent, id, attr, value, type))) {
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
            more_general_node = rete_find_alpha_node(agent, nil, attr, value, type);

        if (! more_general_node && ! SYMBOL_IS_NIL(value))
            more_general_node = rete_find_alpha_node(agent, nil, attr, nil, type);

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
 * Add a wme to or remove a wme from the network.
 */
void
rete_operate_wme(struct agent *agent, struct wme *wme, wme_operation_t op)
{
    int offset = (wme->type == wme_type_normal) ? 0 : 8;
    int i;

#ifdef DEBUG_WMES
    extern struct symtab symtab;
    static void dump_wme(struct symtab *symtab, struct wme *wme);
    printf("%c ", (op == wme_operation_add ? '+' : '-'));
    debug_dump_wme(&symtab, wme);
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

void
rete_dump(struct agent *agent, struct symtab *symtab)
{
    int i;

    printf("\nALPHA NETWORK\n");
    for (i = 0; i < 16; ++i) {
        struct alpha_node *alpha = agent->alpha_nodes[i];
        while (alpha) {
            debug_dump_alpha_node(symtab, alpha, i >= 8);
            alpha = alpha->siblings;
        }
    }

    printf("\nBETA NETWORK\n");
    debug_dump_beta_node(symtab, agent->root_node, 0, 1, 1);
    printf("\n");
}
#endif

