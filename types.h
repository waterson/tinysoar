#ifndef types_h__
#define types_h__

#include "config.h"

typedef unsigned char bool_t;

#define BEGIN_MACRO do {
#define END_MACRO   } while (0)

#define SYMBOL_TYPE_BITS   2
#define SYMBOL_TYPE_SHIFT  (BITS_PER_WORD - SYMBOL_TYPE_BITS)
#define SYMBOL_VALUE_BITS  SYMBOL_TYPE_SHIFT

typedef enum symbol_type {
    symbol_type_variable,
    symbol_type_identifier,
    symbol_type_symbolic_constant,
    symbol_type_integer_constant
} symbol_type_t;

typedef struct symbol {
    int           val  : SYMBOL_VALUE_BITS;
    symbol_type_t type : SYMBOL_TYPE_BITS;
} symbol_t;

#define GET_SYMBOL_VALUE(s)    ((s).val)
#define SET_SYMBOL_VALUE(s, v) ((s).val = (v))
#define GET_SYMBOL_TYPE(s)     ((s).type)
#define SET_SYMBOL_TYPE(s, t)  ((s).type = (t))

#define DECLARE_SYMBOL(s, t) \
  { (s), (t) }

#define MAKE_SYMBOL(s, t, v)     \
  BEGIN_MACRO                    \
  (s).type = (t); (s).val = (v); \
  END_MACRO

#define CLEAR_SYMBOL(s)     \
   BEGIN_MACRO              \
   *((unsigned*) &(s)) = 0; \
   END_MACRO

#define SYMBOLS_ARE_EQUAL(l, r) (*((unsigned*) &(l)) == *((unsigned*) &(r)))

struct symbol_list {
    symbol_t symbol;
    struct symbol_list* next;
};

#endif /* types_h__ */
