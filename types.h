#ifndef types_h__
#define types_h__

#include "config.h"

enum symbol_type;
typedef enum symbol_type symbol_type_t;

enum symbol_type {
    symbol_type_variable,
    symbol_type_identifier,
    symbol_type_symbolic_constant,
    symbol_type_integer_constant
};

struct symbol;
typedef struct symbol symbol_t;

struct symbol {
    int           val  : BITS_PER_WORD - 2;
    symbol_type_t type : 2;
};

#endif /* types_h__ */
