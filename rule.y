%{
#include "soar.h"
#include "parser.h"
#include "symtab.h"
#include <stdlib.h>
#include <string.h>

/* So we'll get verbose error reporting */
#define YYERROR_VERBOSE 1

/* So we'll have a parameter passed in to yyparse() */
#define YYPARSE_PARAM yyparse_param

#if YYDEBUG != 0
extern void yyprint();
#define YYPRINT(stream, token, lval) \
    yyprint((struct parser*) yyparse_param, (stream), (token), (lval))
#endif

/* Prototypes to keep the compiler from whining */
extern void yyerror(char*);

#define YYLEX_PARAM yyparse_param
extern int yylex(void);

struct condition_list {
    struct condition head;
    struct condition* tail;
};
%}

%pure_parser

%union {
    char                context;
    int                 int_constant;
    struct test         test;
    test_type_t         test_type;
    symbol_t            symbol;
    struct rhs_value    rhs_value;
    struct symbol_list* symbol_list;
    struct test_list*   test_list;
    struct condition    condition;
    struct condition*   condition_list;
    bool_t              negated;
    struct action       action;
    struct action*      action_list;
}

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

%type <condition_list> cond_list

%type <negated> opt_negated;

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

%type <test> attr_test
%type <test> conjunctive_test
%type <test> disjunction_test
%type <test> relational_test
%type <test> simple_test
%type <test> test
%type <test> value_test

%type <test_list> simple_test_list

%type <test_type> relation
%%

rule: lhs ARROW rhs
    {
        struct parser* parser =
            (struct parser*) yyparse_param;

        struct production* production =
            parser->production;

        production->conditions =
            (struct condition*) malloc(sizeof(struct condition));

        *(production->conditions) = $1;
        production->actions = $3;
    }
    ;

/*
 * Left-hand side
 */

lhs: cond cond_list
   {
       struct condition* cond = &$1;
       while (cond->next)
           cond = cond->next;

       cond->next = $2;
       $$ = $1;
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
                 struct condition* cond = &$2;
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
             struct condition* new_cond =
                 (struct condition*) malloc(sizeof(struct condition));

             *new_cond = $2;

             if ($1) {
                 /* XXX O(n^2), because we have to get to the tail of
                    the condition list */
                 struct condition* cond = $1;
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
                    struct test_list* context_conjunct;
                    struct test_list* id_test_conjunct;

                    /* Pull up whatever got computed as `id_test', but... */
                    $$ = $3;

                    /* ...we'll need to make a conjunctive `id_test'
                       that incorporates both the original test and
                       the context test. Convert this test to a
                       conjunctive test and fix up the `id_test'
                       slot. */
                    $$.data.simple.id_test.type = test_type_conjunctive;

                    id_test_conjunct =
                        (struct test_list*) malloc(sizeof(struct test_list));

                    id_test_conjunct->test = $3.data.simple.id_test;

                    /* XXX what to do if the test is conjunctive or
                       disjunctive? */
                    if (id_test_conjunct->test.type == test_type_conjunctive ||
                        id_test_conjunct->test.type == test_type_disjunctive) {
                        ERROR(("can't handle conjunctive/disjunctive id test with `%s'",
                               (($2 == 's') ? "state" : "impasse")));
                    }

                    context_conjunct =
                        (struct test_list*) malloc(sizeof(struct test_list));

                    context_conjunct->test.type = ($2 == 's')
                        ? test_type_goal_id
                        : test_type_impasse_id;

                    context_conjunct->test.data.referent = $3.data.simple.id_test.data.referent;

                    $$.data.simple.id_test.data.conjuncts = context_conjunct;
                    context_conjunct->next = id_test_conjunct;
                    id_test_conjunct->next = 0;
                }
                ;

id_test: test
       {
           $$.type = condition_type_positive;
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
                            $<condition>0.data.simple.attr_test = $2.data.simple.attr_test;
                            $<condition>0.data.simple.value_test = $2.data.simple.value_test;
                        }
                        else {
                            struct condition* cond;
                            struct condition* new_cond =
                                (struct condition*) malloc(sizeof(struct condition));

                            /* Pull the attr_ and value_tests from the
                               attr_value_test we just reduced */
                            *new_cond = $2;

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

attr_value_test: opt_negated '^' attr_test dot_attr_list value_test
               {
                   /* XXX is this sufficient to handle negation? */
                   $$.type = $1 ? condition_type_negative : condition_type_positive;
                   $$.data.simple.id_test.type = test_type_blank;
                   $$.data.simple.attr_test = $3;
                   $$.data.simple.value_test = $5;
                   $$.next = 0;

                   /* XXX need to handle dot_attr_list! */
               }
               ;

opt_negated: /* empty */
           { $$ = 0; }
           | '-'
           { $$ = 1; }
           ;

dot_attr_list: /* empty */
             | dot_attr_list '.' attr_test
             {
                 /* XXX */
                 UNIMPLEMENTED();
             }
             ;

attr_test: test
         ;

value_test: test opt_acceptable
          ;

opt_acceptable: /* empty */
              | '+'
              {
                  /* XXX */
                  UNIMPLEMENTED();
              }
              ;

test: conjunctive_test
    | simple_test
    ;

conjunctive_test: '{' simple_test simple_test_list '}'
                {
                    struct test_list* entry =
                        (struct test_list*) malloc(sizeof(struct test_list));

                    entry->test = $2;
                    entry->next = $3;

                    $$.type = test_type_conjunctive;
                    $$.data.conjuncts = entry;
                }
                ;

simple_test_list: simple_test_list simple_test
                {
                    struct test_list* entry =
                        (struct test_list*) malloc(sizeof(struct test_list));

                    entry->test = $2;
                    entry->next = 0;

                    if ($1) {
                        $1->next = entry;
                        $$ = $1;
                    }
                    else $$ = entry;
                }
                | /* empty */
                { $$ = 0; }
                ;

simple_test: disjunction_test
           | relational_test
           ;

disjunction_test: LEFT_ANGLE constants RIGHT_ANGLE
                {
                    struct symbol_list* entry = $2;

                    $$.type = test_type_disjunctive;
                    $$.data.disjuncts = 0;

                    while (entry != 0) {
                        struct symbol_list* doomed;

                        struct test_list* disjunct =
                            (struct test_list*) malloc(sizeof(struct test_list));

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
                       struct action* action = $1;
                       while (action->next)
                           action = action->next;

                       action->next = $2;

                       $$ = $1;
                   }
                   else $$ = $2;
               }
               ;

rhs_action: '(' VARIABLE attr_value_make_list ')'
          {
              struct parser* parser =
                  (struct parser*) yyparse_param;

              struct action* action = $3;

              /* At least one element must be specified in the
                 `attr_value_make_list' */
              if (! action)
                  YYERROR;

              /* Fill in the `id' slot for each of the actions */
              for ( ; action != 0; action = action->next) {
                  action->id.type = rhs_value_type_symbol;
                  action->id.val.symbol = $2;
              }

              $$ = $3;
          }
          ;

attr_value_make_list: /* empty */
                    { $$ = 0; }
                    | attr_value_make_list attr_value_make
                    {
                        if ($1) {
                            struct action* action = $1;

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
                   struct action* action = $3;

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
                       struct action* action = $1;

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
                  struct action* action = $2;

                  /* Fill in the value for each action */
                  for ( ; action != 0; action = action->next)
                      action->value = $1;

                  $$ = $2;
              }
              else {
                  /* There's no preference specifier list; that means
                     that we'll create a default `acceptable'
                     preference action */
                  $$ = (struct action*) malloc(sizeof(struct action));
                  $$->next  = 0;
                  $$->preference_type = preference_type_acceptable;
                  $$->support_type    = 0; /* XXX */
                  $$->value           = $1;
              }
          }
          ;

preference_specifier_list: /* empty */
                         { $$ = 0; }
                         | preference_specifier_list preference_specifier
                         {
                             struct action* new_action =
                                 (struct action*) malloc(sizeof(struct action));

                             *new_action = $2;

                             new_action->next = 0;

                             if ($1) {
                                 /* Append the new action to the list
                                    of actions specified */
                                 struct action* action = $1;

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
                    { $$.preference_type = preference_type_best; } %prec
                    | '='
                    { $$.preference_type = preference_type_unary_indifferent; } %prec
                    | '<'
                    { $$.preference_type = preference_type_worst; } %prec
                    ;

rhs_value: VARIABLE
         {
             $$.type = rhs_value_type_symbol;
             $$.val.symbol = $1;
         }
         | constant
         {
             $$.type = rhs_value_type_symbol;
             $$.val.symbol = $1;
         }
         ;


/*
 * Miscellany
 */

constants: /* empty */
         { $$ = 0; }
         | constants constant
         {
             struct symbol_list* new_entry =
                 (struct symbol_list*) malloc(sizeof(struct symbol_list));

             new_entry->symbol = $2;
             new_entry->next = 0;

             /* XXX do we care if these are maintained in order? */
             if ($1) {
                 struct symbol_list* entry = $1;
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
        { MAKE_SYMBOL($$, symbol_type_integer_constant, $1); }
        ;


%%

#if YYDEBUG != 0
void
yyprint(struct parser* parser, FILE* stream, int token, YYSTYPE lval)
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
