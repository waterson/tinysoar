#include "soar.h"
#include "symtab.h"
#include <stdlib.h>
#include <string.h>

static bool_t
compare_symbols(const struct symtab_entry* e1, const struct symtab_entry* e2)
{
    return e1->symbol.type == e2->symbol.type
        && strcmp(e1->name, e2->name) == 0;
}

static inline unsigned
hash_symbol(const char* name, symbol_type_t type)
{
    unsigned h = (unsigned) type;
    for ( ; *name != 0; ++name)
        h = (h >> (BITS_PER_WORD - 4)) ^ (h << 4) ^ *name;

    return h;
}

static void
add_symbol(struct symtab* symtab, struct ht_entry_header** entryp, const char* name, symbol_t symbol)
{
    struct ht_entry_header* header =
        (struct ht_entry_header*) malloc(sizeof(struct ht_entry_header) + sizeof(struct symtab_entry));

    struct symtab_entry* entry =
        (struct symtab_entry*) HT_ENTRY_DATA(header);

    unsigned hash = hash_symbol(name, symbol.type);

    entry->name   = strdup(name);
    entry->symbol = symbol;

    if (! entryp) {
        struct symtab_entry key;

        key.name = (char*) name;
        key.symbol.type = symbol.type;

        entryp = ht_lookup(&symtab->table, hash, &key);

        ASSERT(! *entryp, ("over-writing symbol table entry for `%s'",
                           ((struct symtab_entry*) HT_ENTRY_DATA(*entryp))->name));
    }

    ht_add(&symtab->table, entryp, hash, header);
}

symbol_t
symtab_lookup(struct symtab* symtab, symbol_type_t type, const char* name, bool_t create)
{
    unsigned hash = hash_symbol(name, type);
    struct ht_entry_header** entryp;
    struct symtab_entry key;
    symbol_t result;

    key.name = (char*) name;
    key.symbol.type = type;

    entryp = ht_lookup(&symtab->table, hash, &key);

    if (*entryp) {
        struct symtab_entry* entry =
            (struct symtab_entry*) HT_ENTRY_DATA(*entryp);

        result = entry->symbol;
    }
    else if (create) {
        unsigned val;

        switch (type) {
        case symbol_type_variable:
            val = symtab->next_variable++;
            break;

        case symbol_type_symbolic_constant:
            val = symtab->next_sym_constant++;
            break;

        case symbol_type_identifier:
            val = symtab->next_identifier++;
            break;

        default:
            /* shouldn't be trying to lookup an integer constant in
               the symbol table, pal. */
            ERROR(("attempt to look up integer constant in symbol table"));
        }

        MAKE_SYMBOL(result, type, val);

        add_symbol(symtab, entryp, name, result);
    }
    else {
        CLEAR_SYMBOL(result);
    }

    return result;
}

struct find_name_closure {
    symbol_t    symbol;
    const char* result;
};

static ht_enumerator_result_t
find_name_enumerator(struct ht_entry_header* header,
                     struct find_name_closure* closure)
{
    struct symtab_entry* entry =
        (struct symtab_entry*) HT_ENTRY_DATA(header);

    if (SYMBOLS_ARE_EQUAL(entry->symbol, closure->symbol)) {
        closure->result = entry->name;
        return ht_enumerator_result_stop;
    }

    return ht_enumerator_result_ok;
}

const char*
symtab_find_name(struct symtab* symtab, symbol_t symbol)
{
    struct find_name_closure closure;
    closure.symbol = symbol;
    closure.result = "(undef)";

    ht_enumerate(&symtab->table,
                 (ht_enumerator_t) find_name_enumerator,
                 &closure);

    return closure.result;
}

struct predefined_symbol {
    symbol_type_t type;
    unsigned      val;
    const char*   name;
};

struct predefined_symbol symbols[] = {
    { symbol_type_symbolic_constant, ATTRIBUTE_CONSTANT,   "attribute" },
    { symbol_type_symbolic_constant, CHOICES_CONSTANT,     "choices" },
    { symbol_type_symbolic_constant, INPUT_LINK_CONSTANT,  "input-link" },
    { symbol_type_symbolic_constant, IO_CONSTANT,          "io" },
    { symbol_type_symbolic_constant, NIL_CONSTANT,         "nil" },
    { symbol_type_symbolic_constant, NONE_CONSTANT,        "none" },
    { symbol_type_symbolic_constant, NO_CHANGE_CONSTANT,   "no-change" },
    { symbol_type_symbolic_constant, OPERATOR_CONSTANT,    "operator" },
    { symbol_type_symbolic_constant, OUTPUT_LINK_CONSTANT, "output-link" },
    { symbol_type_symbolic_constant, QUIESCENCE_CONSTANT,  "quiescence" },
    { symbol_type_symbolic_constant, STATE_CONSTANT,       "state" },
    { symbol_type_symbolic_constant, SUPERSTATE_CONSTANT,  "superstate" },
    { symbol_type_symbolic_constant, TIE_CONSTANT,         "tie" },
    { symbol_type_symbolic_constant, TYPE_CONSTANT,        "type" },
    { symbol_type_symbolic_constant, T_CONSTANT,           "t" },
    { 0, 0, 0 },
};

void
symtab_init(struct symtab* symtab)
{
    struct predefined_symbol* def;

    ht_init(&symtab->table, (ht_key_compare_t) compare_symbols);
    symtab->next_sym_constant = 0;
    symtab->next_variable     = 0;
    symtab->next_identifier   = 0;
    
    for (def = symbols; def->name != 0; ++def) {
        symbol_t sym;
        MAKE_SYMBOL(sym, def->type, def->val);
        add_symbol(symtab, 0, def->name, sym);

        switch (def->type) {
        case symbol_type_symbolic_constant:
            if (def->val > symtab->next_sym_constant)
                symtab->next_sym_constant = def->val;
            break;

        case symbol_type_identifier:
            if (def->val > symtab->next_identifier)
                symtab->next_identifier = def->val;
            break;

        case symbol_type_variable:
            if (def->val > symtab->next_variable)
                symtab->next_variable = def->val;
            break;

        default:
            ERROR(("integer constant in symbol table"));
            break;
        }
    }

    /* Now advance each so that they'll cough up the next available
       number when asked */
    ++symtab->next_sym_constant;
    ++symtab->next_variable;
    ++symtab->next_identifier;
}

static ht_enumerator_result_t
symtab_entry_finalizer(struct ht_entry_header* header, void* closure)
{
    struct symtab_entry* entry =
        (struct symtab_entry*) HT_ENTRY_DATA(header);

    free(entry->name);
    return ht_enumerator_result_delete;
}

void
symtab_finish(struct symtab* symtab)
{
    ht_finish(&symtab->table, symtab_entry_finalizer, 0);
}
