#ifndef wmem_h__
#define wmem_h__

#include "types.h"

struct wme;
typedef struct wme wme_t;

struct wme {
    symbol_t object;
    symbol_t attribute;
    symbol_t value;
};

#endif /* wmem_h__ */
