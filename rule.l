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
 * Flex lexer for Soar productions
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
    struct parser *parser;
%}

ID [[:alpha:]]([[:alnum:]]|"-"|"_"|"*")*

%%

state         { yylval->context = yytext; return CONTEXT; }

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

[+-]?[0-9]+   yylval->int_constant = atoi(yytext); return INT_CONSTANT;

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

struct production *
soar_parse_rule(struct symtab *symtab, const char *rule)
{
    extern int yyparse(void *);
    struct parser parser;
    YY_BUFFER_STATE state;

    struct symbol_list *symbols;

    struct production *result =
        (struct production *) malloc(sizeof(struct production));

#if YYDEBUG != 0
    /* Turn on noisy debugging */
    extern int yydebug;
    yydebug = 1;
#endif

    result->conditions       = 0;
    result->actions          = 0;
    result->instantiations   = 0;
    result->num_unbound_vars = 0;
    result->support          = support_type_isupport;

    parser.symtab           = symtab;
    parser.production       = result;
    parser.lhs_vars         = 0;
    parser.rhs_unbound_vars = 0;
    parser.parsed_name      = 0;
    parser.gensym           = 0;

    state = yy_scan_string(rule);
    if (yyparse(&parser) != 0) {
        free(result);
        result = 0;
    }
    yy_delete_buffer(state);

    symbols = parser.lhs_vars;
    while (symbols) {
        struct symbol_list *doomed = symbols;
        symbols = symbols->next;
        free(doomed);
    }

    symbols = parser.rhs_unbound_vars;
    while (symbols) {
        struct symbol_list *doomed = symbols;
        symbols = symbols->next;
        free(doomed);
    }

    return result;
}

void
yyerror(const char *s)
{
    fprintf(stderr, "%s\n", s);
}
