#ifndef parser_h__
#define parser_h__

#include "soar.h"

struct preference_specifier_list {
    struct preference_specifier_list* next;
    preference_type_t type;
    struct rhs_value  referent;
}; 

struct parser {
    struct symtab*     symtab;
    struct production* production;
};


extern struct production*
soar_parse_rule(struct symtab* symtab, const char* rule);

#endif /* parser_h__ */
