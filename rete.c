#include "alloc.h"
#include "wmem.h"
#include "rete.h"

struct wme_list {
    wme_t*           wme;
    struct wme_list* next;
};

struct alpha_node {
    symbol_t id;
    symbol_t attr;
    symbol_t value;
    struct alpha_node* siblings;
    struct beta_node*  children;
    struct wme_list*   wmes;
};

struct beta_test {
    struct test test;
    struct beta_test* next;
};

typedef enum beta_node_type {
    beta_node_type_root,
    beta_node_type_memory,
    beta_node_type_positive_join,
    beta_node_type_production
} beta_node_type_t;

struct beta_node {
    beta_node_type_t type;
    struct beta_node* siblings;
    struct beta_node* children;

    union {
        struct {
            struct production* production;
        } p;
    } data;
};

struct rete {
    struct beta_node root;
};

void
rete_init(struct rete* rete)
{
    rete->root.type = beta_node_type_root;
}

static inline struct beta_node*
new_beta_node()
{
    return (struct beta_node*) alloc(sizeof(struct beta_node));
}

static inline struct beta_test*
new_beta_test()
{
    return (struct beta_test*) alloc(sizeof(struct beta_test));
}

/*
 * Convert a `test' from a condition into the appropriate RETE
 * structures: constants that are tested by the alpha network and
 * nodes in the beta network.
 */
static void
process_test(const struct test* test, symbol_t* constant)
{
    switch (test->type) {
    case test_type_blank:
        return;

    case test_type_equality:
        if (GET_SYMBOL_TYPE(test->data.referent) != symbol_type_variable) {
            /* It's a constant. Install an alpha test if possible;
               otherwise, create a beta test node */
            if (! GET_SYMBOL_VALUE(*constant)) {
                *constant = test->data.referent;
            }
            else {
                /* XXX */
            }
        }
        else {
            /* It's a variable. Make a variable equality test */
            struct beta_test* beta_test = new_beta_test();
        }
        return;

    default:
        assert(0); /* XXX */
    }
}

/*
 * Find or create a beta node for the given single condition `cond',
 * which must be a simple positive condition. The new node is a child
 * of `parent'.
 */
static struct beta_node*
create_positive_condition_node(struct rete* net,
                               const struct condition* cond,
                               struct beta_node* parent)
{
    symbol_t alpha_id, alpha_attr, alpha_value;
    process_test(&cond->data.simple.id_test, &alpha_id);
    process_test(&cond->data.simple.attr_test, &alpha_attr);
    process_test(&cond->data.simple.value_test, &alpha_value);
    return 0;
}


void
rete_add_production(struct rete* net, const struct production* p)
{
    struct beta_node* parent = &net->root;
    struct condition* cond;

    for (cond = p->lhs; cond != 0; cond = cond->next) {
        struct beta_node* child;

        switch (cond->type) {
        case condition_type_positive:
            child = create_positive_condition_node(net, cond, parent);
            break;

        case condition_type_negative:
        case condition_type_conjunctive_negation:
            break;
        }

        assert(child != 0);
        parent = child;
    }
}

void
rete_remove_production()
{
}

void
rete_add_wme()
{
}

void
rete_remove_wme()
{
}
