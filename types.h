#ifndef types_h__
#define types_h__

#include "config.h"

typedef int variable_t;
typedef int symbol_t;
typedef int object_t;

typedef enum value_type {
    value_type_variable,
    value_type_object,
    value_type_symbol,
    value_type_integer
} value_type_t;

typedef struct value {
    value_type_t type : 2;
    int          val  : BITS_PER_WORD - 2;
} value_t;

static inline int
value_equals(const value_t* left, const value_t* right)
{
    return *((int*)left) == *((int*)right);
}

#define MAKE_VARIABLE_VALUE(_var) { value_type_variable, (_var) }
#define MAKE_OBJECT_VALUE(_obj)   { value_type_object,   (_obj) }
#define MAKE_SYMBOL_VALUE(_sym)   { value_type_symbol,   (_sym) }
#define MAKE_INTEGER_VALUE(_n)    { value_type_integer,  (_n)   }

#define OBJECT_STATE 1

#endif /* types_h__ */
