%{
/* #define YYDEBUG 1 */
#include "soar.h"
#include "parser.h"
#include "symtab.h"

#define YYPARSE_PARAM yyparse_param

static void
process_attr_value_test_list(struct condition* cond,
                             struct attr_value_test_list* list);
%}

%union {
    const char*                  sym_constant;
    int                          int_constant;
    struct test                  test;
    test_type_t                  test_type;
    symbol_t                     symbol;
    struct rhs_value             rhs_value;
    struct symbol_list*          symbol_list;
    struct test_list*            test_list;
    struct condition             condition;
    struct condition*            condition_list;
    struct attr_value_test       attr_value_test;
    struct attr_value_test_list* attr_value_test_list;
}

%token CONTEXT

%token LEFT_ANGLE
%token RIGHT_ANGLE

%token ARROW

%token NOT_EQUAL
%token LESS_THAN_OR_EQUAL
%token GREATER_THAN_OR_EQUAL
%token SAME_TYPE

%token <sym_constant> VARIABLE
%token <sym_constant> SYM_CONSTANT
%token <int_constant> INT_CONSTANT

%type <test> conjunctive_test
%type <test> id_test
%type <test> value_test
%type <test> attr_test
%type <test> test
%type <test> simple_test
%type <test> relational_test
%type <test> disjunction_test
%type <attr_value_test> attr_value_test
%type <test_list> simple_test_list
%type <attr_value_test_list> attr_value_test_list
%type <test_type> relation
%type <symbol> single_test
%type <symbol> constant
%type <rhs_value> rhs_value
%type <rhs_value> value_make
%type <symbol_list> constants
%type <condition> conds_for_one_id
%type <condition> positive_cond
%type <condition> cond
%type <condition> lhs
%type <condition_list> cond_list
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
    }
    ;

/*
 * Left-hand side
 */

lhs: cond cond_list
   {
       $1.next = $2;
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
                 $$ = $2;
                 $2.next = $3;
             }
             ;

cond_list: cond_list cond
         {
             struct condition* cond =
                 (struct condition*) malloc(sizeof(struct condition));

             *cond = $2;
             cond->next = 0;
             $1->next = cond;
             $$ = $1;
         }
         | /* empty */
         { $$ = 0; }
         ;

conds_for_one_id: '(' id_test attr_value_test_list ')'
                {
                    $$.type = condition_type_positive;
                    $$.data.simple.id_test = $2;

                    process_attr_value_test_list(&$$, $3);
                }

                | '(' CONTEXT id_test attr_value_test_list ')'
                {
                    struct test_list* context_conjunct;
                    struct test_list* id_test_conjunct;

                    $$.type = condition_type_positive;
                    $$.data.simple.id_test.type = test_type_conjunctive;

                    id_test_conjunct =
                        (struct test_list*) malloc(sizeof(struct test_list));

                    id_test_conjunct->test = $3;

                    context_conjunct =
                        (struct test_list*) malloc(sizeof(struct test_list));

                    context_conjunct->test.type = test_type_goal_id;
                    context_conjunct->test.data.referent = $3.data.referent;

                    $$.data.simple.id_test.data.conjuncts = context_conjunct;
                    context_conjunct->next = id_test_conjunct;
                    id_test_conjunct->next = 0;

                    process_attr_value_test_list(&$$, $4);
                }
                ;

id_test: test
       ;

attr_value_test_list: /* empty */
                    { $$ = 0; }
                    | attr_value_test_list attr_value_test
                    {
                        struct attr_value_test_list* entry =
                            (struct attr_value_test_list*) malloc(sizeof(struct test_list));

                        entry->tests = $2;
                        entry->next = 0;

                        if ($1) {
                            $1->next = entry;
                            $$ = $1;
                        }
                        else {
                            $$ = entry;
                        }
                    }
                    ;

attr_value_test: '^' attr_test dot_attr_list value_test
               {
                   /* XXX need to handle dot_attr_list! */
                   $$.attr_test = $2;
                   $$.value_test = $4;
               }
               ;

dot_attr_list: /* empty */
             | dot_attr_list '.' attr_test
             ;

attr_test: test
         ;

value_test: test opt_acceptable
          ;

opt_acceptable: /* empty */
              | '+'
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

                    $1->next = entry;

                    $$ = $1;
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
           {
               struct parser* parser =
                   (struct parser*) yyparse_param;

               $$ = symtab_lookup(parser->symtab, symbol_type_variable, $1, 1);
           }
           | constant
           ;

/*
 * Right-hand side
 */

rhs: rhs_action_list
   ;

rhs_action_list: /* empty */
               | rhs_action_list rhs_action
               ;

rhs_action: '(' VARIABLE attr_value_make attr_value_make_list ')'
          ;

attr_value_make_list: /* empty */
                    | attr_value_make_list attr_value_make
                    ;

attr_value_make: '^' rhs_value value_make value_make_list
               ;


value_make_list: /* empty */
               | value_make_list value_make
               ;

value_make: rhs_value preference_specifier_list
          ;

preference_specifier_list: /* empty */
                         | preference_specifier_list preference_specifier
                         ;

preference_specifier: '+'
                    | '-'
                    | '!'
                    | '~'
                    | '@'
                    | '=' %prec
                    | '>' rhs_value
                    | '=' rhs_value
                    | '<' rhs_value
                    ;

rhs_value: VARIABLE
         {
             /* XXX */
             $$.type = rhs_value_type_symbol;
             /* or maybe rhs_value_type_unbound_variable */
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
             $$ = (struct symbol_list*) malloc(sizeof(struct symbol_list));
             $$->symbol = $2;
             $$->next = 0;
             $1->next = $$;
         }
         ;

constant: SYM_CONSTANT
        {
            struct parser* parser =
                (struct parser*) yyparse_param;

             $$ = symtab_lookup(parser->symtab, symbol_type_symbolic_constant, $1, 1);
        }
        | INT_CONSTANT
        { MAKE_SYMBOL($$, symbol_type_integer_constant, $1); }
        ;


%%

static void
process_attr_value_test_list(struct condition* cond,
                             struct attr_value_test_list* list)
{
    if (! list) {
        cond->data.simple.attr_test.type = test_type_blank;
        cond->data.simple.value_test.type = test_type_blank;
    }
    else if (! list->next) {
        cond->data.simple.attr_test  = list->tests.attr_test;
        cond->data.simple.value_test = list->tests.value_test;
    }
    else {
        /* XXX writeme */
    }
}
