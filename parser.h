#ifndef parser_h__
#define parser_h__

#include "soar.h"

struct parser {
    struct symtab*      symtab;
    struct production*  production;
    struct symbol_list* lhs_vars;
    struct symbol_list* rhs_unbound_vars;
    bool_t              parsed_name;
};


extern struct production*
soar_parse_rule(struct symtab* symtab, const char* rule);

#endif /* parser_h__ */
