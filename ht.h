#ifndef ht_h__
#define ht_h__

#include "config.h"

struct ht_entry {
    struct ht_entry* next;
    unsigned hash;
    const void* key;
    void* value;
};

typedef bool_t (*ht_key_comparitor_t)(const void* k1, const void* k2);

typedef enum ht_enumerator_result {
    ht_enumerator_result_ok,
    ht_enumerator_result_stop,
    ht_enumerator_result_delete
} ht_enumerator_result_t;

typedef ht_enumerator_result_t (*ht_enumerator_t)(struct ht_entry* entry, void* closure);

struct ht {
    struct ht_entry** buckets;
    unsigned short shift;
    unsigned short nentries;
    ht_key_comparitor_t compare_keys;
};

void
ht_init(struct ht* ht, ht_key_comparitor_t compare_keys);

struct ht_entry**
ht_lookup(struct ht* ht, unsigned hash, const void* key);

void
ht_add(struct ht* ht, struct ht_entry** bucket, unsigned hash, const void* key, void* value);

void
ht_remove(struct ht* ht, struct ht_entry** bucket);

void
ht_enumerate(struct ht* ht, ht_enumerator_t enumerator, void* closure);

#endif /* ht_h__ */
