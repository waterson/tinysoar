#ifndef ht_h__
#define ht_h__

#include "config.h"

/*
 * Hashtable entry header. The user allocates the hashtable entry, the
 * two words of which are used for hashtable bookkeeping.
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

/*
 * Hashtable structure.
 */
struct ht {
    struct ht_entry_header **buckets;
    unsigned short           shift;
    unsigned short           nentries;
    ht_key_compare_t         compare_keys;
};

/*
 * Initialize a hashtable.
 */
void
ht_init(struct ht *ht, ht_key_compare_t compare_keys);

/*
 * Lookup an entry in a hashtable.
 *
 * The caller must compute the entry's hash; ht_lookup will search the
 * appropriate bucket's chain looking for an entry whose key matches
 * the specified key. The hashtable's `compare_keys' function is used
 * to test for key equality.
 *
 * ht_lookup returns a link pointer that must be dereferenced once to
 * get the pointer to the entry. The link pointer will never be null;
 * but the dereferenced link pointer may be null if the entry wasn't
 * found.
 *
 * The link pointer may be passed to ht_add or ht_remove.
 */
struct ht_entry_header **
ht_lookup(struct ht *ht, unsigned hash, const void *key);

/*
 * Add an entry to the hashtable. The caller must allocate the entry,
 * compute the entry's hash, and call ht_lookup to get an appropriate
 * link pointer.
 */
void
ht_add(struct ht *ht, struct ht_entry_header **bucket, unsigned hash, struct ht_entry_header *entry);

/*
 * Remove an entry from the hashtable. The link pointer to the entry
 * must be acquired using ht_lookup. This does not release any storage
 * that was allocated for the entry.
 */
void
ht_remove(struct ht *ht, struct ht_entry_header **bucket);

/*
 * Return codes for a hashtable enumerator function:
 *
 * ht_enumerator_result_ok
 *   Continue enumeration.
 *
 * ht_enumerator_result_stop
 *   Halt enumeration and return immediately.
 *
 * ht_enumerator_result_delete
 *   Continue enumeration after removing the entry from the
 *   hashtable. Note that storage for the entry is not released.
 */
typedef enum ht_enumerator_result {
    ht_enumerator_result_ok,
    ht_enumerator_result_stop,
    ht_enumerator_result_delete
} ht_enumerator_result_t;

/*
 * A hashtable enumerator function. This function is called once per
 * entry, and is passed the entry and the caller's closure as
 * arguments.
 */
typedef ht_enumerator_result_t (*ht_enumerator_t)(struct ht_entry_header *header, void *closure);

/*
 * Enumerate the entries in a hashtable, calling the specified
 * enumerator function once for each entry. The closure is passed
 * through to the enumerator function.
 */
void
ht_enumerate(struct ht *ht, ht_enumerator_t enumerator, void *closure);

/*
 * Clean up the storage associated with a hashtable. Note that this
 * will not release the storage allocated for the entries. If
 * specified, the entry_finalizer is called once per entry with the
 * closure argument; this routine can be used to free the storage used
 * for entries.
 */
void
ht_finish(struct ht *ht, ht_enumerator_t entry_finalizer, void *closure);

#endif /* ht_h__ */
