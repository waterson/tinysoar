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

/*
 * Find or create a beta node for the given single condition `cond',
 * which must be a simple positive condition. The new node is a child
 * of `parent'.
 */
static struct beta_node*
create_positive_condition_node(struct rete* net,
                               struct condition* cond,
                               struct beta_node* parent)
{
    return 0;
}


void
rete_add_production(struct rete* net, struct production* p)
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
