#ifndef parser_h__
#define parser_h__

#include "soar.h"

struct parser {
    struct symtab*     symtab;
    struct production* production;
};


extern struct production*
soar_parse_rule(struct symtab* symtab, const char* rule);

#endif /* parser_h__ */
