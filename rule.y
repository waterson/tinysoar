%{ /* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
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
 * Bison grammar for Soar productions.
 *
 * TO DO
 * -----
 *
 * . Error recovery and reporting.
 *
 * . Implement semantic verification (e.g., Are there dangling
 *   variables in the LHS? Does the RHS try to create disconnected
 *   structure?).
 *
 * . Clean up condition and condition_list stuff. I think the RHS stuff
 *   worked out much more cleanly. Better yet, construct the RETE
 *   network directly instead of creating a bunch of intermediate data
 *   structures (unless it turns out we need these for chunking?)
 *
 * . Implement `negated conjunctive conditions'. Properly detect when
 *   we need to return a simple condition vs. an NCC.
 *
 * . Fix O(n^2) list walking that's all over the place.
 *
 * . Probably plenty of memory leaks in here.
 *
 */

#include "soar.h"
#include "parser.h"
#include "symtab.h"
#include <stdlib.h>
#include <string.h>

static bool_t
is_operator_test(struct test *test);

/*
 * So we'll get verbose error reporting if YYERROR is defined
 */
#define YYERROR_VERBOSE 1

/*
 * So we'll have a parameter passed in to yyparse(). This'll end up
 *  being a `struct parser *'
 */
#define YYPARSE_PARAM yyparse_param
#define YYLEX_PARAM   yyparse_param

/*
 * A fancier printing routine that's savvy to our symbol table.
 */
#if YYDEBUG != 0
extern void yyprint();
#define YYPRINT(stream, token, lval) \
    yyprint((struct parser *) yyparse_param, (stream), (token), (lval))
#endif

/*
 * Prototypes to keep the compiler from whining
 */
extern void yyerror(char *);
extern int yylex(); /* Can't give it types, because we don't have
                       YYLVAL defined yet. */
%}

/*
 * Make it so we're re-entrant. This actually isn't so important as
 * just being able to pass a parameter into yylex()!
 */
%pure_parser

/*
 * There are thirteen shift-reduce conflicts; specifically, due to the
 * fact that we can parser a |CONTEXT| as a |constant|, and due to
 * several of the |preference_specifier| non-terminals.
 */
%expect 13

/*
 * The YYSTYPE
 */
%union {
    char               *name;
    char               *context;
    struct test         test;
    test_type_t         test_type;
    struct test_list   *test_list;
    struct condition    condition;
    struct condition   *condition_list;
    bool_t              negated;
    bool_t              acceptable;
    symbol_t            symbol;
    int                 int_constant;
    struct rhs_value    rhs_value;
    struct symbol_list *symbol_list;
    struct action       action;
    struct action      *action_list;
}

%token <name> NAME
%type <name> name

%token LEFT_ANGLE
%token RIGHT_ANGLE

%token ARROW

%token NOT_EQUAL
%token LESS_THAN_OR_EQUAL
%token GREATER_THAN_OR_EQUAL
%token SAME_TYPE

%token <context> CONTEXT

%token <symbol> VARIABLE
%token <symbol> SYM_CONSTANT

%token <int_constant> INT_CONSTANT

%type <condition> attr_value_test
%type <condition> attr_value_test_list
%type <condition> cond
%type <condition> conds_for_one_id
%type <condition> id_test
%type <condition> lhs
%type <condition> positive_cond

%type <test_list> dot_attr_list

%type <condition_list> cond_list

%type <test> attr_test
%type <test> conjunctive_test
%type <test> disjunction_test
%type <test> relational_test
%type <test> simple_test
%type <test> test
%type <test> value_test

%type <test_list> simple_test_list

%type <test_type> relation

%type <negated> opt_negated;
%type <acceptable> opt_acceptable;

%type <rhs_value> rhs_variable
%type <rhs_value> rhs_value
%type <action> preference_specifier;
%type <action_list> preference_specifier_list;
%type <action_list> value_make
%type <action_list> attr_value_make
%type <action_list> value_make_list
%type <action_list> rhs_action
%type <action_list> attr_value_make_list
%type <action_list> rhs_action_list
%type <action_list> rhs

%type <symbol> constant
%type <symbol> single_test

%type <symbol_list> constants
%%

rule: name lhs ARROW rhs
    {
        struct parser *parser =
            (struct parser *) yyparse_param;

        struct production *production =
            parser->production;

        production->conditions =
            (struct condition *) malloc(sizeof(struct condition));

        *(production->conditions) = $2;
        production->actions = $4;
    }
    ;

name: NAME
    {
        struct parser *parser = 
            (struct parser *) yyparse_param;

        parser->parsed_name = 1;

#ifdef DEBUG
        parser->production->name = strdup($1);
#endif
    }
    ;

/*
 * Left-hand side
 */

lhs: cond cond_list
   {
       struct condition *cond = &$1;
       while (cond->next)
           cond = cond->next;

       cond->next = $2;
       $$ = $1;

       /* XXX at this point, we should verify that the rule's
          conditions form a tree, not a forest. (In other words, check
          that there are no `disconnected' tests.) */
   }
   ;

cond: positive_cond
    | '-' positive_cond
    {
        $$ = $2; /* XXX negate! */
    }
    ;

positive_cond: conds_for_one_id
             | '{' cond cond_list '}'
             {
                 /* XXX O(n^2), because we have to get to the tail of
                    the condition list */
                 struct condition *cond = &$2;
                 while (cond->next)
                     cond = cond->next;

                 cond->next = $3;
                 $$ = $2;
             }
             ;

cond_list: /* empty */
         { $$ = 0; }
         | cond_list cond
         {
             struct condition *new_cond =
                 (struct condition *) malloc(sizeof(struct condition));

             *new_cond = $2;

             if ($1) {
                 /* XXX O(n^2), because we have to get to the tail of
                    the condition list */
                 struct condition *cond = $1;
                 while (cond->next)
                     cond = cond->next;

                 cond->next = new_cond;
                 $$ = $1;
             }
             else $$ = new_cond;
         }
         ;

conds_for_one_id: '(' id_test attr_value_test_list ')'
                {
                    $$ = $2;
                }

                | '(' CONTEXT id_test attr_value_test_list ')'
                {
                    struct test_list *context_conjunct;
                    struct test_list *id_test_conjunct;

                    /* Pull up whatever got computed as `id_test', but... */
                    $$ = $3;

                    /* ...we'll need to make a conjunctive `id_test'
                       that incorporates both the original test and
                       the context test. Convert this test to a
                       conjunctive test and fix up the `id_test'
                       slot. */
                    $$.data.simple.id_test.type = test_type_conjunctive;

                    id_test_conjunct =
                        (struct test_list *) malloc(sizeof(struct test_list));

                    id_test_conjunct->test = $3.data.simple.id_test;

                    /* XXX what to do if the test is conjunctive or
                       disjunctive? */
                    if (id_test_conjunct->test.type == test_type_conjunctive ||
                        id_test_conjunct->test.type == test_type_disjunctive) {
                        ERROR(("can't handle conjunctive/disjunctive id test with `state'"));
                    }

                    context_conjunct =
                        (struct test_list *) malloc(sizeof(struct test_list));

                    context_conjunct->test.type = test_type_goal_id;
                    context_conjunct->test.data.referent = $3.data.simple.id_test.data.referent;

                    $$.data.simple.id_test.data.conjuncts = context_conjunct;
                    context_conjunct->next = id_test_conjunct;
                    id_test_conjunct->next = 0;
                }
                ;

id_test: test
       {
           $$.type = condition_type_positive;
           $$.acceptable = 0;
           $$.data.simple.id_test = $1;
           $$.data.simple.attr_test.type = test_type_blank;
           $$.data.simple.value_test.type = test_type_blank;
           $$.next = 0;
       }
       ;

attr_value_test_list: /* empty */
                    { /* nothing to do */ }
                    | attr_value_test_list attr_value_test
                    {
                        if ($<condition>0.data.simple.attr_test.type == test_type_blank &&
                            $<condition>0.data.simple.value_test.type == test_type_blank) {
                            /* There's room in the id_test to our left
                               for the attribute and value tests. */
                            struct condition *cond;

                            $<condition>0.data.simple.attr_test = $2.data.simple.attr_test;
                            $<condition>0.data.simple.value_test = $2.data.simple.value_test;

                            /* XXX I'm a little bit nervous about
                               whacking the type and acceptable flag
                               like this, but...

                               Copy the test type (positive, negative)
                               from the attribute-value test to
                               correctly propagate negative
                               tests. Same with the condition's
                               acceptable flag. */
                            $<condition>0.type = $2.type;
                            $<condition>0.acceptable = $2.acceptable;

                            /* Copy any other conditions that may have
                               been trailing along, as well. */
                            cond = &$<condition>0;
                            while (cond->next)
                                cond = cond->next;

                            cond->next = $2.next;
                        }
                        else {
                            struct condition *cond;
                            struct condition *new_cond =
                                (struct condition *) malloc(sizeof(struct condition));

                            /* Pull the attr_ and value_tests from the
                               attr_value_test we just reduced */
                            *new_cond = $2;

                            /* Copy the id_test from the test to our
                               left. */
                            new_cond->data.simple.id_test = $<condition>0.data.simple.id_test;

                            /* Add this condition to the end of the
                               list of conditions that we're reducing */
                            cond = &$<condition>0;

                            /* XXX O(n^2), because we have to get to
                               the tail of the condition list */
                            while (cond->next)
                                cond = cond->next;

                            cond->next = new_cond;
                        }
                    }
                    ;

attr_value_test: opt_negated '^' attr_test dot_attr_list value_test opt_acceptable
               {
                   struct parser *parser = (struct parser *) yyparse_param;
                   struct condition *cond = &$$;

                   /* Our `id' test will be computed after we're
                      reduced. Our first `attr' test is immediately
                      available. */
                   cond->data.simple.id_test.type = test_type_blank;
                   cond->data.simple.attr_test = $3;
                   cond->next = 0;

                   /* If we're not creating a negative condition, not
                      testing acceptable preferences and we are
                      testing an ^operator attribute, then mark the
                      production as generating o-supported
                      preferences. (This seems a bit too liberal, but
                      I think it's how Soar8 works.) */
                   if (!$1 && !$6 && is_operator_test(&$3))
                       parser->production->support = support_type_osupport;

                   /* Now walk through the `.attr' list, creating a
                      chain of equality tests. */
                   while ($4) {
                       char symbuf[16];
                       struct test_list *doomed;
                       struct condition *prev = cond;
                       symbol_t link;

                       /* Create a dummy symbol: Soar symbols can't
                          start with `?', so this ought to be
                          unique. */
                       sprintf(symbuf, "?%d", ++parser->gensym);
                       link = symtab_lookup(parser->symtab,
                                            symbol_type_variable,
                                            symbuf, 1);

                       /* The previous test's `value' test will bind
                          the link variable. */
                       prev->data.simple.value_test.type = test_type_equality;
                       prev->data.simple.value_test.data.referent = link;

                       /* The current test's `id' test will bind the
                          link variable, and we'll pick up the `attr'
                          from the list we reduced. */
                       cond = (struct condition *) malloc(sizeof(struct condition));
                       cond->data.simple.id_test.type = test_type_equality;
                       cond->data.simple.id_test.data.referent = link;
                       cond->data.simple.attr_test = $4->test;
                       cond->next = 0;

                       prev->next = cond;

                       /* Check for o-support along the chain. */
                       if (!$6 && is_operator_test(&$4->test))
                           parser->production->support = support_type_osupport;

                       /* Clean up after ourselves. */
                       doomed = $4;
                       $4 = $4->next;
                       free(doomed);
                   }

                   /* Now add the `value' test at the end of the
                      chain. This is where we'll apply negation and
                      the `acceptable' test. */
                   cond->data.simple.value_test = $5;

                   /* XXX is this sufficient to handle negation? */
                   cond->type = $1 ? condition_type_negative : condition_type_positive;
                   cond->acceptable = $6;
               }
               ;

opt_negated: /* empty */
           { $$ = 0; }
           | '-'
           { $$ = 1; }
           ;

dot_attr_list: /* empty */
             { $$ = 0; }
             | dot_attr_list '.' attr_test
             {
                 /* Collect the attribute tests into a list. */
                 struct test_list *entry =
                     (struct test_list *) malloc(sizeof(struct test_list));

                 entry->test = $3;
                 entry->next = 0;

                 if ($1) {
                     $1->next = entry;
                     $$ = $1;
                 }
                 else
                     $$ = entry;
             }
             ;

attr_test: test
         ;

value_test: test
          | /* empty */
          {
              $$.type = test_type_blank;
          }
          ;

opt_acceptable: /* empty */
              { $$ = 0; }
              | '+'
              { $$ = 1; }
              ;

test: conjunctive_test
    | simple_test
    ;

conjunctive_test: '{' simple_test simple_test_list '}'
                {
                    struct test_list *entry =
                        (struct test_list *) malloc(sizeof(struct test_list));

                    entry->test = $2;
                    entry->next = $3;

                    $$.type = test_type_conjunctive;
                    $$.data.conjuncts = entry;
                }
                ;

simple_test_list: simple_test_list simple_test
                {
                    struct test_list *entry =
                        (struct test_list *) malloc(sizeof(struct test_list));

                    entry->test = $2;
                    entry->next = 0;

                    if ($1) {
                        $1->next = entry;
                        $$ = $1;
                    }
                    else
                        $$ = entry;
                }
                | /* empty */
                { $$ = 0; }
                ;

simple_test: disjunction_test
           | relational_test
           ;

disjunction_test: LEFT_ANGLE constants RIGHT_ANGLE
                {
                    struct symbol_list *entry = $2;

                    $$.type = test_type_disjunctive;
                    $$.data.disjuncts = 0;

                    while (entry != 0) {
                        struct symbol_list *doomed;

                        struct test_list *disjunct =
                            (struct test_list *) malloc(sizeof(struct test_list));

                        disjunct->test.type = test_type_equality;
                        disjunct->test.data.referent = entry->symbol;
                        disjunct->next = $$.data.disjuncts;
                        $$.data.disjuncts = disjunct;

                        doomed = entry;
                        entry = entry->next;
                        free(doomed);
                    }
                }
                ;

relational_test: relation single_test
               {
                   $$.type = $1;
                   $$.data.referent = $2;
               }
               ;

relation: /* empty */
        { $$ = test_type_equality; }
        | '='
        { $$ = test_type_equality; }
        | NOT_EQUAL
        { $$ = test_type_not_equal; }
        | '<'
        { $$ = test_type_less; }
        | '>'
        { $$ = test_type_greater; }
        | LESS_THAN_OR_EQUAL
        { $$ = test_type_less_or_equal; }
        | GREATER_THAN_OR_EQUAL
        { $$ = test_type_greater_or_equal; }
        | SAME_TYPE
        { $$ = test_type_same_type; }
        ;

single_test: VARIABLE
           {
               /* Note that the variable appears in the LHS. */
               struct parser *parser =
                   (struct parser *) yyparse_param;

               struct symbol_list *entry;
               struct symbol_list **link;

               for (link = &parser->lhs_vars; (entry = *link) != 0; link = &entry->next) {
                   if (SYMBOLS_ARE_EQUAL(entry->symbol, $1))
                       break;
               }

               if (! entry) {
                   entry = (struct symbol_list *) malloc(sizeof(struct symbol_list));
                   entry->symbol = $1;
                   entry->next   = 0;

                   *link = entry;
               }

               $$ = $1;
           }
           | constant
           ;

/*
 * Right-hand side
 */

rhs: rhs_action_list
   ;

rhs_action_list: /* empty */
               { $$ = 0; }
               | rhs_action_list rhs_action
               {
                   if ($1) {
                       struct action *action = $1;
                       while (action->next)
                           action = action->next;

                       action->next = $2;

                       $$ = $1;
                   }
                   else $$ = $2;
               }
               ;

rhs_action: '(' rhs_variable attr_value_make_list ')'
          {
              struct action *action = $3;

              /* At least one element must be specified in the
                 `attr_value_make_list' */
              if (! action)
                  YYERROR;

              /* Fill in the `id' slot for each of the actions */
              for ( ; action != 0; action = action->next)
                  action->id = $2;

              $$ = $3;
          }
          ;

attr_value_make_list: /* empty */
                    { $$ = 0; }
                    | attr_value_make_list attr_value_make
                    {
                        if ($1) {
                            struct action *action = $1;

                            while (action->next)
                                action = action->next;

                            action->next = $2;

                            $$ = $1;
                        }
                        else $$ = $2;
                    }
                    ;

attr_value_make: '^' rhs_value value_make_list
               {
                   struct action *action = $3;

                   /* There must be at least one action in the
                      `value_make_list' */
                   if (! action)
                       YYERROR;

                   /* Fill in the `attr' slot for each of the actions */
                   for ( ; action != 0; action = action->next)
                       action->attr = $2;

                   $$ = $3;
               }
               ;


value_make_list: /* empty */
               { $$ = 0; }
               | value_make_list value_make
               {
                   if ($1) {
                       struct action *action = $1;

                       while (action->next)
                           action = action->next;

                       action->next = $2;

                       $$ = $1;
                   }
                   else $$ = $2;
               }
               ;

value_make: rhs_value preference_specifier_list
          {
              if ($2) {
                  /* There's a preference specifier list. */
                  struct action *action = $2;

                  /* Fill in the value for each action */
                  for ( ; action != 0; action = action->next)
                      action->value = $1;

                  $$ = $2;
              }
              else {
                  /* There's no preference specifier list; that means
                     that we'll create a default `acceptable'
                     preference action */
                  $$ = (struct action *) malloc(sizeof(struct action));
                  $$->next            = 0;
                  $$->preference_type = preference_type_acceptable;
                  $$->value           = $1;
              }
          }
          ;

preference_specifier_list: /* empty */
                         { $$ = 0; }
                         | preference_specifier_list preference_specifier
                         {
                             struct action *new_action =
                                 (struct action *) malloc(sizeof(struct action));

                             *new_action = $2;

                             new_action->next = 0;

                             if ($1) {
                                 /* Append the new action to the list
                                    of actions specified */
                                 struct action *action = $1;

                                 while (action->next)
                                     action = action->next;

                                 action->next = new_action;

                                 $$ = $1;
                             }
                             else $$ = new_action;
                         }
                         ;

preference_specifier: '+'
                    { $$.preference_type = preference_type_acceptable; }
                    | '-'
                    { $$.preference_type = preference_type_reject; }
                    | '!'
                    { $$.preference_type = preference_type_require; }
                    | '~'
                    { $$.preference_type = preference_type_prohibit; }
                    | '@'
                    { $$.preference_type = preference_type_reconsider; }
                    | '>' rhs_value
                    { $$.preference_type = preference_type_better; $$.referent = $2; }
                    | '=' rhs_value
                    { $$.preference_type = preference_type_binary_indifferent; $$.referent = $2; }
                    | '<' rhs_value
                    { $$.preference_type = preference_type_worse; $$.referent = $2; }
                    | '>' 
                    { $$.preference_type = preference_type_best; }
                    | '='
                    { $$.preference_type = preference_type_unary_indifferent; }
                    | '<'
                    { $$.preference_type = preference_type_worst; }
                    ;

rhs_value: rhs_variable
         | constant
         {
             $$.type = rhs_value_type_symbol;
             $$.val.symbol = $1;
         }
         ;

rhs_variable: VARIABLE
            {
                struct parser *parser =
                    (struct parser *) yyparse_param;

                struct symbol_list *entry;

                /* Is this a bound variable from the lhs? */
                for (entry = parser->lhs_vars; entry != 0; entry = entry->next) {
                    if (SYMBOLS_ARE_EQUAL(entry->symbol, $1))
                        break;
                }

                if (entry) {
                    /* Yep, it's bound in the LHS. */
                    $$.type = rhs_value_type_symbol;
                    $$.val.symbol = $1;
                }
                else {
                    /* It's unbound */
                    unsigned index = 0;
                    struct symbol_list **link;

                    $$.type = rhs_value_type_unbound_variable;

                    /* Is it an unbound RHS variable we already know
                       about? */
                    for (link = &parser->rhs_unbound_vars; (entry = *link) != 0; link = &entry->next) {
                        if (SYMBOLS_ARE_EQUAL(entry->symbol, $1))
                            break;

                        ++index;
                    }

                    if (! entry) {
                        /* Nope, we've never seen this variable
                           before. Remember it. */
                        ++parser->production->num_unbound_vars;

                        entry = (struct symbol_list *) malloc(sizeof(struct symbol_list));
                        entry->symbol = $1;
                        entry->next = 0;

                        *link = entry;
                    }

                    /* Whether new or not, `index' will contain the
                       proper index for the unbound variable. */
                    $$.val.unbound_variable = index;
                }
            }


/*
 * Miscellany
 */

constants: /* empty */
         { $$ = 0; }
         | constants constant
         {
             struct symbol_list *new_entry =
                 (struct symbol_list *) malloc(sizeof(struct symbol_list));

             new_entry->symbol = $2;
             new_entry->next = 0;

             /* XXX do we care if these are maintained in order? If
                not, no need to walk the list. */
             if ($1) {
                 struct symbol_list *entry = $1;
                 while (entry->next)
                     entry = entry->next;

                 entry->next = new_entry;
                 $$ = $1;
             }
             else $$ = new_entry;
         }
         ;

constant: SYM_CONSTANT
        | INT_CONSTANT
        { INIT_SYMBOL($$, symbol_type_integer_constant, $1); }
        | CONTEXT /* hack to pull `state' through as a constant */
        {
            struct parser *parser = (struct parser *) yyparse_param;
            $$ = symtab_lookup(parser->symtab,
                               symbol_type_symbolic_constant,
                               $1, 1);
        }
        ;


%%

#if YYDEBUG != 0
void
yyprint(struct parser *parser, FILE *stream, int token, YYSTYPE lval)
{
    switch (token) {
    case SYM_CONSTANT:
    case VARIABLE:
        fprintf(stream, " `%s'",
                symtab_find_name(parser->symtab, lval.symbol));

        break;

    default:
        /* do nothing */
        break;
    }
}
#endif /* YYDEBUG */

static bool_t
is_operator_test(struct test *test)
{
    switch (test->type) {
    case test_type_equality:
        return SYMBOLS_ARE_EQUAL(test->data.referent, SYM(OPERATOR_CONSTANT));

    case test_type_conjunctive:
    case test_type_disjunctive: {
        /* If any of the conjuncts or disjuncts test for ^operator,
           we'll call this an ``operator test''. Too liberal? */
        struct test_list *tests;

        /* |struct test|'s |conjuncts| and |disjuncts| fields overlap,
           so we'll just use |conjuncts| regardless. */
        for (tests = test->data.conjuncts; tests != 0; tests = tests->next) {
            if (is_operator_test(&tests->test))
                return 1;
        }
    }
    break;

    default:
        break;
    }

    return 0;
}
