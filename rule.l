/* -*- Mode: C; indent-tabs-mode: nil -*- */

/*

  Flex lexer for Soar productions

*/

/*
 * We don't need and don't have yywrap()
 */
%option noyywrap

%{
#include "soar.h"
#include "parser.h"
#include "symtab.h"
#include "y.tab.h"
#include <stdlib.h>

/*
 * Since the parser is built to be re-entrant, we need to supply our
 * YY_DECL.
 */
#define YY_DECL            \
int yylex (yylval, parser) \
    YYSTYPE* yylval;       \
    struct parser* parser;
%}

ID [[:alpha:]]([[:alnum:]]|"-"|"_"|"*")*

%%

state|impasse yylval->context = yytext[0]; return CONTEXT;

"<"{ID}">"    { yylval->symbol =
                  symtab_lookup(parser->symtab,
                                symbol_type_variable, 
                                yytext, 1);
                return VARIABLE; }

{ID}          { if (parser->parsed_name) {
                     yylval->symbol =
                         symtab_lookup(parser->symtab,
                                       symbol_type_symbolic_constant,
                                       yytext, 1);
                     return SYM_CONSTANT;
                 }
                 else {
                     yylval->name = yytext;
                     return NAME;
                 }
              }

[0-9]+        yylval->int_constant = atoi(yytext); return INT_CONSTANT;

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

    struct symbol_list* symbols;

    struct production* result =
        (struct production*) malloc(sizeof(struct production));

#if YYDEBUG != 0
    /* Turn on noisy debugging */
    extern int yydebug;
    yydebug = 1;
#endif

    result->conditions       = 0;
    result->actions          = 0;
    result->instantiations   = 0;
    result->num_unbound_vars = 0;

    parser.symtab           = symtab;
    parser.production       = result;
    parser.lhs_vars         = 0;
    parser.rhs_unbound_vars = 0;
    parser.parsed_name      = 0;

    state = yy_scan_string(rule);
    if (yyparse(&parser) != 0) {
        free(result);
        result = 0;
    }
    yy_delete_buffer(state);

    symbols = parser.lhs_vars;
    while (symbols) {
        struct symbol_list* doomed = symbols;
        symbols = symbols->next;
        free(doomed);
    }

    symbols = parser.rhs_unbound_vars;
    while (symbols) {
        struct symbol_list* doomed = symbols;
        symbols = symbols->next;
        free(doomed);
    }

    return result;
}

void
yyerror(const char* s)
{
    fprintf(stderr, "%s\n", s);
}
