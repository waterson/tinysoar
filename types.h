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

#define SYMBOL_TYPE_BITS  2
#define SYMBOL_VALUE_BITS (BITS_PER_WORD - SYMBOL_TYPE_BITS)

#ifdef HAS_C_PACKED_FIELDS
struct symbol;
typedef struct symbol symbol_t;

struct symbol {
    int           val  : SYMBOL_VALUE_BITS;
    symbol_type_t type : SYMBOL_TYPE_BITS;
};

#define GET_SYMBOL_TYPE(s)     ((s).type)
#define SET_SYMBOL_TYPE(s, t)  (((s).type) = (t))
#define GET_SYMBOL_VALUE(s)    ((s).val)
#define SET_SYMBOL_VALUE(s, v) (((s).val) = (v))

#define MAKE_SYMBOL(s, t, v) do { (s).type = (t); (s).val = (v); } while (0)
#else /* !HAS_C_PACKED_FIELDS */
typedef unsigned symbol_t;

#define SYMBOL_VALUE_MASK ((1 << SYMBOL_VALUE_BITS) - 1)
#define SYMBOL_TYPE_MASK  (~SYMBOL_VALUE_MASK)

#define GET_SYMBOL_TYPE(s)      ((s) >> SYMBOL_VALUE_BITS)

#define SET_SYMBOL_TYPE(s, t)   (((s) &= SYMBOL_VALUE_MASK),\
                                 ((s) |= ((t) << SYMBOL_VALUE_BITS)))

#define GET_SYMBOL_VALUE(s)     ((s) & SYMBOL_VALUE_MASK)

#define SET_SYMBOL_VALUE(s, v)  (((s) &= SYMBOL_TYPE_MASK),\
                                 ((s) |= (v)))

#define MAKE_SYMBOL(s, t, v) do { (s) = ((t) << SYMBOL_VALUE_BITS) | (s) } while (0)

#endif /* !HAS_C_PACKED_FIELDS */

#endif /* types_h__ */
