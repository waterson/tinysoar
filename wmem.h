#ifndef wmem_h__
#define wmem_h__

#include "types.h"
#include "pool.h"

typedef enum wme_type {
    wme_type_normal,
    wme_type_acceptable_preference
} wme_type_t;

struct wme {
    symbol_t id;
    symbol_t attr;
    symbol_t value;
    unsigned bits; /* low bit is this wme's type; high bits are pointer
                      to next wme */
};

#define WME_TYPE_MASK ((unsigned)(0x1))
#define WME_NEXT_MASK ((unsigned)(~0x1))

#define GET_WME_TYPE(w)    ((wme_type_t)((w).bits & WME_TYPE_MASK))
#define SET_WME_TYPE(w, t) ((w).bits &= ((unsigned)(t)) | WME_NEXT_MASK)

#define GET_WME_NEXT(w)    ((struct wme*)((w).bits & WME_NEXT_MASK))
#define SET_WME_NEXT(w, n) ((w).bits &= ((unsigned)(n)) | WME_TYPE_MASK)

struct wmem {
    struct wme* wmes;
    struct pool wme_pool;
};

extern void
wmem_init(struct wmem* wmem);

extern struct wme*
wmem_add(struct wmem* wmem, symbol_t id, symbol_t attr, symbol_t value, wme_type_t type);

#endif /* wmem_h__ */
