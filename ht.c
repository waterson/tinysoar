/*
 * Multiplicative hash, from Knuth 6.4. Borrows heavily from Mozilla's
 * NSPR hashtable implementation, see
 *
 *   http://lxr.mozilla.org/mozilla/source/nsprpub/lib/ds/plhash.h
 *   http://lxr.mozilla.org/mozilla/source/nsprpub/lib/ds/plhash.c
 *
 */

#include "ht.h"
#include "alloc.h"
#include "config.h"

#if SIZEOF_INT >= 4
#  define GOLDEN_RATIO 0x9E3779B9U
#else
#  define GOLDEN_RATIO 0x79B9U /* XXX is this right? */
#endif

/* The minimum number of buckets we'll allow a hashtable to have */
#define MINBUCKETS_LOG2 3
#define MINBUCKETS      (1 << MINBUCKETS_LOG2)

/* Compute how many buckets the current table has */
#define NBUCKETS(ht) (1 << (BITS_PER_WORD - (ht)->shift))

#define MAX_LOAD(n)  ((n) - ((n) >> 3))
#define MIN_LOAD(n)  (((n) > MINBUCKETS) ? ((n) >> 2) : 0)

static void
rehash(struct ht* ht, struct ht_entry** oldbuckets, unsigned noldbuckets)
{
    int i;

    for (i = 0; i < noldbuckets; ++i) {
        struct ht_entry* oldentry = oldbuckets[i];
        while (oldentry != 0) {
            struct ht_entry* next = oldentry->next;
            struct ht_entry** newbucket =
                ht_lookup(ht, oldentry->hash, HT_ENTRY_DATA(oldentry));
            ASSERT(*newbucket == 0, ("corrupted hashtable"));
            oldentry->next = 0;
            *newbucket = oldentry;
            oldentry = next;
        }
    }
}

void
ht_init(struct ht* ht, ht_key_comparitor_t compare_keys)
{
    int i;

    ht->buckets = malloc(MINBUCKETS * sizeof(struct ht_entry*));
    ASSERT(ht->buckets != 0, ("out of memory"));

    ht->shift = BITS_PER_WORD - MINBUCKETS_LOG2;
    ht->nentries = 0;
    ht->compare_keys = compare_keys;

    for (i = NBUCKETS(ht) - 1; i >= 0; --i)
        ht->buckets[i] = 0;
}

void
ht_finish(struct ht* ht)
{
    free(ht->buckets);
}

struct ht_entry**
ht_lookup(struct ht* ht, unsigned hash, const void* key)
{
    struct ht_entry** bucket;
    struct ht_entry* entry;
    unsigned h;

    h = hash * GOLDEN_RATIO;
    h >>= ht->shift;

    bucket = &ht->buckets[h];
    while ((entry = *bucket) != 0) {
        if (entry->hash == hash && (*ht->compare_keys)(key, HT_ENTRY_DATA(entry)))
            break;

        bucket = &entry->next;
    }

    return bucket;
}

void
ht_add(struct ht* ht, struct ht_entry** bucket, unsigned hash, struct ht_entry* entry)
{
    unsigned nbuckets = NBUCKETS(ht);

    if (ht->nentries > MAX_LOAD(nbuckets)) {
        /* overloaded */
        struct ht_entry** oldbuckets = ht->buckets;
        int newnbuckets = 2 * nbuckets;
        int i;

        ht->shift--;
        ht->buckets = malloc(newnbuckets * sizeof(struct ht_entry*));
        ASSERT(ht->buckets != 0, ("out of memory"));

        /* zero the new table */
        for (i = 0; i < newnbuckets; ++i)
            ht->buckets[i] = 0;

        /* re-hash old values */
        rehash(ht, oldbuckets, nbuckets);
        free(oldbuckets);

        /* make sure `bucket' is sane */
        bucket = ht_lookup(ht, hash, HT_ENTRY_DATA(entry));
    }

    /* link the entry into the hashtable */
    entry->next  = *bucket;
    entry->hash  = hash;
    *bucket = entry;

    ++ht->nentries;
}

void
ht_remove(struct ht* ht, struct ht_entry** entryp)
{
    struct ht_entry* doomed = *entryp;
    unsigned nbuckets = NBUCKETS(ht);

    *entryp = doomed->next;

    if (--ht->nentries < MIN_LOAD(nbuckets)) {
        /* underloaded */
        struct ht_entry** oldbuckets = ht->buckets;
        int newnbuckets = nbuckets / 2;
        int i;

        ht->shift++;
        ht->buckets = malloc(newnbuckets * sizeof(struct ht_entry*));
        ASSERT(ht->buckets != 0, ("out of memory"));

        /* zero the new table */
        for (i = 0; i < newnbuckets; ++i)
            ht->buckets[i] = 0;

        /* rehash old values */
        rehash(ht, oldbuckets, nbuckets);
        free(oldbuckets);
    }
}

void
ht_enumerate(struct ht* ht, ht_enumerator_t enumerator, void* closure)
{
    unsigned nbuckets = NBUCKETS(ht);
    int i;

    for (i = nbuckets - 1; i >= 0; --i) {
        struct ht_entry** entryp = &ht->buckets[i];
        struct ht_entry* entry = *entryp;

        while (entry != 0) {
            ht_enumerator_result_t result =
                (*enumerator)(entry, closure);

            if (result == ht_enumerator_result_ok) {
                entryp = &entry->next;
                entry = entry->next;
            }
            else if (result == ht_enumerator_result_delete) {
                struct ht_entry* doomed = entry;
                entry = entry->next;
                *entryp = entry;
                free(doomed);
                --ht->nentries;
            }
            else {
                ASSERT(result == ht_enumerator_result_stop, ("bad ht_enumerator_result"));
                return;
            }
        }        
    }
}
