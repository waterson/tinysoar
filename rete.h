#ifndef rete_h__
#define rete_h__

#include "types.h"
#include "wmem.h"

typedef struct wme_set {
    wme_t* wme;
    struct wme_set* next;
} wme_set_t;



typedef struct binding {
    variable_t            variable;
    value_t               value;
    const struct binding* parent;
    struct binding*       next;
} binding_t;

typedef enum node_type {
    node_type_simple_test,
    node_type_complex_test,
    node_type_join,
    node_type_instantiation
} node_type_t;

typedef struct node {
    node_type_t  type;
    struct node* siblings;
    binding_t*   bindings;
} node_t;

typedef struct inner_node {
    node_t       node;
    struct node* children;
} inner_node_t;

typedef struct simple_test_node {
    inner_node_t inner;
    variable_t   object;
    symbol_t     attribute;
    value_t      value;
} simple_test_node_t;

typedef enum complex_test_type {
    complex_test_type_state_object,
    complex_test_type_impasse_object
} complex_test_type_t;

typedef struct complex_test_node {
    inner_node_t        inner;
    complex_test_type_t test;
    int                 data;
} complex_test_node_t;

typedef struct join_node {
    inner_node_t inner;
    variable_t   left;
    variable_t   right;
} join_node_t;

typedef struct instantiation_node {
    node_t     node;
    char*      name; /* XXX */
} instantiation_node_t;

typedef struct rete {
    inner_node_t* root;
} rete_t;

void
rete_init(rete_t* rete, wme_t* wmes);

void
rete_add_wme(rete_t* rete, wme_t* wme);

#endif /* rete_h__ */
