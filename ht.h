#ifndef ht_h__
#define ht_h__

#include "config.h"

/*
 * Hashtable entry header. The user allocates the hashtable entry, the
 * first few bytes of which are used for hashtable bookkeeping.
 */
struct ht_entry_header {
    struct ht_entry_header *next;
    unsigned                hash;
};

/*
 * Retrieve the data from a hashtable entry by skipping over the
 * header.
 */
#define HT_ENTRY_DATA(e) (((char *) e) + sizeof(struct ht_entry_header))

/*
 * Compare hashtable entry `keys'. The routine should return non-zero
 * if the keys are equal.
 */
typedef bool_t (*ht_key_compare_t)(const void *k1, const void *k2);

struct ht {
    struct ht_entry_header **buckets;
    unsigned short           shift;
    unsigned short           nentries;
    ht_key_compare_t         compare_keys;
};

void
ht_init(struct ht *ht, ht_key_compare_t compare_keys);

struct ht_entry_header **
ht_lookup(struct ht *ht, unsigned hash, const void *key);

void
ht_add(struct ht *ht, struct ht_entry_header **bucket, unsigned hash, struct ht_entry_header *entry);

void
ht_remove(struct ht *ht, struct ht_entry_header **bucket);

typedef enum ht_enumerator_result {
    ht_enumerator_result_ok,
    ht_enumerator_result_stop,
    ht_enumerator_result_delete
} ht_enumerator_result_t;

typedef ht_enumerator_result_t (*ht_enumerator_t)(struct ht_entry_header *header, void *closure);

void
ht_enumerate(struct ht *ht, ht_enumerator_t enumerator, void *closure);

void
ht_finish(struct ht *ht, ht_enumerator_t entry_finalizer, void *closure);

#endif /* ht_h__ */
