#ifndef symtab_h__
#define symtab_h__

#include "ht.h"
#include "soar.h"

struct symtab_entry {
    char*    name;
    symbol_t symbol;
};

struct symtab {
    struct ht table;
    unsigned next_sym_constant;
    unsigned next_identifier;
    unsigned next_variable;
};

extern void
symtab_init(struct symtab* symtab);

extern symbol_t
symtab_lookup(struct symtab* symtab, symbol_type_t type, const char* name, bool_t create);

extern const char*
symtab_find_name(struct symtab* symtab, symbol_t symbol);

#endif /* symtab_h__ */
