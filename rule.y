%{
#define YYDEBUG 1
%}

%token CONTEXT

%token LEFT_ANGLE
%token RIGHT_ANGLE

%token ARROW

%token NOT_EQUAL
%token LESS_THAN_OR_EQUAL
%token GREATER_THAN_OR_EQUAL
%token SAME_TYPE

%token VARIABLE
%token SYM_CONSTANT
%token INT_CONSTANT

%%

rule: lhs ARROW rhs
    ;

/*
 * Left-hand side
 */

lhs: cond cond_list
   ;

cond: positive_cond
    | '-' positive_cond
    ;

positive_cond: conds_for_one_id
             | '{' cond cond_list '}'
             ;

cond_list: cond_list cond
         | /* empty */
         ;

conds_for_one_id: '(' id_test ')'
                | '(' CONTEXT id_test ')'
                | '(' attr_value_test attr_value_test_list ')'
                | '(' CONTEXT attr_value_test attr_value_test_list ')'
                | '(' id_test attr_value_test attr_value_test_list ')'
                | '(' CONTEXT id_test attr_value_test attr_value_test_list ')'
                ;

id_test: test
       ;

attr_value_test_list: /* empty */
                    | attr_value_test_list attr_value_test
                    ;

attr_value_test: '^' attr_test dot_attr_list value_test
               ;

dot_attr_list: /* empty */
             | dot_attr_list '.' attr_test
             ;

attr_test: test
         ;

value_test: test opt_plus
          ;

opt_plus: /* empty */
        | '+'
        ;

test: conjunctive_test
    | simple_test
    ;

conjunctive_test: '{' simple_test simple_test_list '}'
                ;

simple_test_list: simple_test_list simple_test
                | /* empty */
                ;

simple_test: disjunction_test
           | relational_test
           ;

disjunction_test: LEFT_ANGLE constants RIGHT_ANGLE
                ;

relational_test: relation single_test
               ;

relation: /* empty */
        | '='
        | NOT_EQUAL
        | '<'
        | '>'
        | LESS_THAN_OR_EQUAL
        | GREATER_THAN_OR_EQUAL
        | SAME_TYPE
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
         | constant
         ;


/*
 * Miscellany
 */

constants: /* empty */
         | constants constant
         ;

constant: SYM_CONSTANT
        | INT_CONSTANT
        ;


%%
