/* -*- Mode: C -*- */
%option noyywrap

%{
#include "soar.h"
#include "parser.h"
#include "y.tab.h"
#include <stdlib.h>
%}

ID [[:alpha:]]([[:alnum:]]|"-"|"_"|"*")*

%%

state|impasse yylval.context = yytext[0]; return CONTEXT;
"<"{ID}">"    yylval.sym_constant = yytext; return VARIABLE;
{ID}          yylval.sym_constant = yytext; return SYM_CONSTANT;
[0-9]+        yylval.int_constant = atoi(yytext); return INT_CONSTANT;
"<<"          return LEFT_ANGLE;
">>"          return RIGHT_ANGLE;
"<>"          return NOT_EQUAL;
"<="          return LESS_THAN_OR_EQUAL;
">="          return GREATER_THAN_OR_EQUAL;
"<=>"         return SAME_TYPE;
"-->"         return ARROW;
"("|")"|"-"|"+"|"."|"^"|"{"|"}"|"="|"<"|">"|"!"|"~"|"@" return yytext[0];
[[:space:]]+  /* consume whitespace */

%%

struct production*
soar_parse_rule(struct symtab* symtab, const char* rule)
{
    extern int yyparse(void*);
    struct parser parser;
    YY_BUFFER_STATE state;

    struct production* result =
        (struct production*) malloc(sizeof(struct production));

#if YYDEBUG != 0
    extern int yydebug;
    yydebug = 1;
#endif

    result->conditions       = 0;
    result->actions          = 0;
    result->instantiations   = 0;
    result->num_unbound_vars = 0;

    parser.symtab     = symtab;
    parser.production = result;

    state = yy_scan_string(rule);
    if (yyparse(&parser) != 0) {
        free(result);
        result = 0;
    }
    yy_delete_buffer(state);
    return result;
}

void
yyerror(const char* s)
{
    fprintf(stderr, "%s\n", s);
}