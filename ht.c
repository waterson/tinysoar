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

#if SIZEOF_INT == 4
#  define GOLDEN_RATIO 0x9E3779B9U
#elif SIZEOF_INT == 2
#  define GOLDEN_RATIO 0x9E37U
#else
#  error "unsupported integer size"
#endif

/*
 * The minimum number of buckets we'll allow a hashtable to have.
 */
#define MINBUCKETS_LOG2 3
#define MINBUCKETS      (1 << MINBUCKETS_LOG2)

/*
 * Compute how many buckets the current table has
 */
#define NBUCKETS(ht) (1 << (BITS_PER_WORD - (ht)->shift))

/*
 * Compute the maximum acceptable load for a table of size `n'.
 */
#define MAX_LOAD(n)  ((n) - ((n) >> 3))

/*
 * Compute the minimum acceptable load for a table of size `n'.
 */
#define MIN_LOAD(n)  (((n) > MINBUCKETS) ? ((n) >> 2) : 0)


static void
rehash(struct ht *ht, struct ht_entry_header **oldbuckets, unsigned noldbuckets)
{
    int i;

    for (i = noldbuckets - 1; i >= 0; --i) {
        struct ht_entry_header *oldentry = oldbuckets[i];
        while (oldentry) {
            struct ht_entry_header *next = oldentry->next;
            struct ht_entry_header **newbucket =
                ht_lookup(ht, oldentry->hash, HT_ENTRY_DATA(oldentry));
            ASSERT(*newbucket == 0, ("corrupted hashtable"));
            oldentry->next = 0;
            *newbucket = oldentry;
            oldentry = next;
        }
    }
}

void
ht_init(struct ht *ht, ht_key_compare_t compare_keys)
{
    int i;

    ht->buckets = malloc(MINBUCKETS * sizeof(struct ht_entry_header *));
    ASSERT(ht->buckets != 0, ("out of memory"));

    ht->shift = BITS_PER_WORD - MINBUCKETS_LOG2;
    ht->nentries = 0;
    ht->compare_keys = compare_keys;

    for (i = NBUCKETS(ht) - 1; i >= 0; --i)
        ht->buckets[i] = 0;
}

void
ht_finish(struct ht *ht, ht_enumerator_t entry_finalizer, void *closure)
{
    ht_enumerate(ht, entry_finalizer, closure);
    free(ht->buckets);
}

struct ht_entry_header **
ht_lookup(struct ht *ht, unsigned hash, const void *key)
{
    struct ht_entry_header **bucket, *entry;
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
ht_add(struct ht *ht, struct ht_entry_header **bucket, unsigned hash, struct ht_entry_header *entry)
{
    unsigned nbuckets = NBUCKETS(ht);

    if (ht->nentries > MAX_LOAD(nbuckets)) {
        /* Overloaded. Grow the table. */
        struct ht_entry_header **oldbuckets = ht->buckets;
        int newnbuckets = 2 * nbuckets;
        int i;

        ht->shift--;
        ht->buckets = malloc(newnbuckets * sizeof(struct ht_entry_header *));
        ASSERT(ht->buckets != 0, ("out of memory"));

        /* Zero the new table. */
        for (i = newnbuckets - 1; i >= 0; --i)
            ht->buckets[i] = 0;

        /* Re-hash old values. */
        rehash(ht, oldbuckets, nbuckets);
        free(oldbuckets);

        /* Make sure `bucket' is sane. */
        bucket = ht_lookup(ht, hash, HT_ENTRY_DATA(entry));
    }

    /* Link the entry into the hashtable. */
    entry->next = *bucket;
    entry->hash = hash;
    *bucket = entry;

    ++ht->nentries;
}

void
ht_remove(struct ht *ht, struct ht_entry_header **entryp)
{
    struct ht_entry_header *doomed = *entryp;
    unsigned nbuckets = NBUCKETS(ht);

    *entryp = doomed->next;

    if (--ht->nentries < MIN_LOAD(nbuckets)) {
        /* Underloaded. Shrink the table. */
        struct ht_entry_header **oldbuckets = ht->buckets;
        int newnbuckets = nbuckets / 2;
        int i;

        ht->shift++;
        ht->buckets = malloc(newnbuckets * sizeof(struct ht_entry_header *));
        ASSERT(ht->buckets != 0, ("out of memory"));

        /* Zero the new table. */
        for (i = 0; i < newnbuckets; ++i)
            ht->buckets[i] = 0;

        /* Rehash old values. */
        rehash(ht, oldbuckets, nbuckets);
        free(oldbuckets);
    }
}

void
ht_enumerate(struct ht *ht, ht_enumerator_t enumerator, void *closure)
{
    int nbuckets = NBUCKETS(ht);
    int i;

    for (i = nbuckets - 1; i >= 0; --i) {
        struct ht_entry_header **link = &ht->buckets[i], *entry;

        while ((entry = *link) != 0) {
            ht_enumerator_result_t result =
                (*enumerator)(entry, closure);

            switch (result) {
            case ht_enumerator_result_ok:
                link = &entry->next;
                break;

            case ht_enumerator_result_delete:
                *link = entry->next;
                free(entry);
                --ht->nentries;
                break;

            default:
                ASSERT(result == ht_enumerator_result_stop, ("bad ht_enumerator_result"));
                return;
            }
        }        
    }
}
