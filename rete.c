/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*

  The Rete Network


  TO DO
  -----

  . Implement conjunctive negative conditions

*/

#include "soar.h"
#include "rete.h"
#include "alloc.h"

/*
 * Create a new token
 */
static inline struct token*
create_token(struct beta_node* node,
             struct token*     parent,
             struct wme*       wme)
{
    struct token* result = (struct token*) malloc(sizeof(struct token));
    result->parent = parent;
    result->node   = node;
    result->wme    = wme;
    return result;
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
 * Add a wme to the specified alpha node's right memory
 */
/*static inline*/ void
add_wme_to_alpha_node(struct agent*      agent,
                      struct alpha_node* node,
                      struct wme*        wme)
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
static inline void
remove_wme_from_alpha_node(struct agent*      agent,
                           struct alpha_node* node,
                           struct wme*        wme)
{
    struct right_memory** link = &node->right_memories;
    struct right_memory* rm = *link;

    while (rm) {
        if (rm->wme == wme) {
            *link = rm->next_in_alpha_node;
            free(rm);
            break;
        }

        link = &rm->next_in_alpha_node;
        rm = *link;
    }
}

/*
 * Check a single beta test
 */
static bool_t
check_beta_test(struct agent*     agent,
                struct beta_test* test,
                struct token*     token,
                struct wme*       wme)
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
                    /* Start with |depth == 1| referring to the WME
                       from the token. */
                    struct token* t = token;

                    /* Walk up the token for anything deeper. */
                    while (--depth >= 1)
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
check_beta_tests(struct agent*     agent,
                 struct beta_test* test,
                 struct token*     token,
                 struct wme*       wme)
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
do_left_addition(struct agent*     agent,
                 struct beta_node* node,
                 struct token*     token,
                 struct wme*       wme)
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
do_left_removal(struct agent*     agent,
                struct beta_node* node,
                struct token*     token,
                struct wme*       wme)
{
    switch (node->type) {
    case beta_node_type_memory:
        {
            struct token* doomed;
            struct token **link;

            for (link = &node->tokens, doomed = *link;
                 doomed != 0;
                 link = &doomed->next, doomed = *link) {
                if ((doomed->wme == wme) && (doomed->parent == token)) {
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
                if ((doomed->wme == wme) && (doomed->parent == token)) {
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
                    if ((doomed->wme == wme) && (doomed->parent == token)) {
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
                struct instantiation* inst;
                for (inst = node->data.production->instantiations;
                     inst != 0;
                     inst = inst->next) {
                    if ((inst->token->wme == wme) && (inst->token->parent == token)) {
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
                            match->production         = node->data.production;
                            match->next               = agent->retractions;

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

/*
 * Notify the beta node |node| that |wme| is about to be removed from
 * the right-memory to which |node| is attached.
 */
static inline void
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


