/* -*- Mode: C -*- */
%option noyywrap

%{
#include "soar.h"
#include "parser.h"
#include "rule.tab.h"
%}

%%

state|impasse return CONTEXT;
"<<" return LEFT_ANGLE;
">>" return RIGHT_ANGLE;
"<>" return NOT_EQUAL;
"<=" return LESS_THAN_OR_EQUAL;
">=" return GREATER_THAN_OR_EQUAL;
"<=>" return SAME_TYPE;
"-->" return ARROW;
[0-9]+ yylval.int_constant = atoi(yytext); return INT_CONSTANT;
"<"[[:alnum:]]+">" return VARIABLE;
[[:alnum:]]+ return SYM_CONSTANT;
"("|")"|"-"|"+"|"."|"^"|"{"|"}"|"="|"<"|">"|"!"|"~"|"@" return yytext[0];
[ \t\n]+    /* consume whitespace */

%%

struct production*
soar_parse_rule(const char* rule)
{
    YY_BUFFER_STATE state;
    struct production* result =
        (struct production*) malloc(sizeof(struct production));

#if YY_DEBUG != 0
    extern int yydebug;
    yydebug = 1;
#endif

    result->conditions       = 0;
    result->actions          = 0;
    result->instantiations   = 0;
    result->num_unbound_vars = 0;

    state = yy_scan_string(rule);
    if (yyparse(result) != 0) {
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
