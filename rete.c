#include <assert.h>
#include <malloc.h>

#include "rete.h"

static const value_t*
get_binding_for(const binding_t* binding, variable_t var)
{
    for ( ; binding != 0; binding = binding->parent) {
        if (binding->variable == var)
            return &binding->value;
    }

    return 0;
}

static void
extend_binding(binding_t** bindings, const binding_t* parent, variable_t var, const value_t* value)
{
    binding_t* binding = (binding_t*) malloc(sizeof(binding_t));
    assert(binding);

    binding->variable = var;
    binding->value    = *value;
    binding->parent   = parent;

    if (*bindings)
        (*bindings)->next = binding;

    *bindings = binding;
}

static int
simple_test(simple_test_node_t* node, const binding_t* bindings, wme_t* wme)
{
    if (node->attribute != wme->attribute)
        return 1;

    if (get_binding_for(bindings, node->object)) {
        /* If we've already bound the object variable, then this
           reduces to a consitency check. */
        const binding_t* parent;
        for (parent = bindings; parent != 0; parent = parent->next) {
            const value_t* obj = get_binding_for(parent, node->object);

            /* Every binding should have the object variable bound! */
            assert(obj != 0);

            if (node->value.type == value_type_variable) {
                /* We're binding a variable. Is it consistent? */
                const value_t* value = get_binding_for(parent, node->value.val);
                if (!value || (value->val == wme->value.val)) {
                    /* Yep. */
                    extend_binding(&node->inner.node.bindings, parent, node->value.val, &wme->value);
                }
            }
            else {
                /* It's a simple consistency check. */
                if (value_equals(&node->value, &wme->value)) {
                    extend_binding(&node->inner.node.bindings, parent, node->object, obj);
                }
            }
        }
    }
    else {
        /* The object variable is free. See if the WME will fit */
        if (node->value.type == value_type_variable) {
            /* XXX ouch. we have to bind both the object and the value
               variables, but there's no way for our data structure to
               do that! */
            assert(0);
        }
        else {
            if (value_equals(&node->value, &wme->value)) {
                if (bindings) {
                    const binding_t* parent;
                    for (parent = bindings; parent != 0; parent = parent->next)
                        extend_binding(&node->inner.node.bindings, parent, node->object, &wme->value);
                }
                else {
                    extend_binding(&node->inner.node.bindings, 0, node->object, &wme->value);
                }
            }
        }
    }
    return 1;
}

static int
complex_test(complex_test_node_t* node, const binding_t* bindings, wme_t* wme)
{
    if (node->test == complex_test_type_state_object) {
        if (wme->object == OBJECT_STATE) { /* XXX wrong! can be >1 state object */
            binding_t* binding;
            for (binding = node->inner.node.bindings; binding != 0; binding = binding->next) {
                assert(binding->value.type == value_type_object);
                if (binding->value.val == wme->object)
                    return 1;
            }

            if (bindings) {
                const binding_t* parent;
                for (parent = bindings; parent != 0; parent = parent->next) {
                    value_t val = MAKE_OBJECT_VALUE(wme->object);
                    extend_binding(&node->inner.node.bindings, parent, (variable_t) node->data, &val);
                }
            }
            else {
                value_t val = MAKE_OBJECT_VALUE(wme->object);
                extend_binding(&node->inner.node.bindings, 0, (variable_t) node->data, &val);
            }
        }

        return 1;
    }

    return 0;
}

static int
join(join_node_t* node, wme_t* wme)
{
    return 0;
}

static void
instantiation(instantiation_node_t* node, wme_t* wme)
{
}

static void
add_wme(inner_node_t* node, binding_t* bindings, wme_t* wme)
{
    for ( ; node != 0; node = (inner_node_t*) node->node.siblings) {
        switch (node->node.type) {
        case node_type_simple_test:
            if (! simple_test((simple_test_node_t*) node, bindings, wme))
                continue;

            break;

        case node_type_complex_test:
            if (! complex_test((complex_test_node_t*) node, bindings, wme))
                continue;

            break;

        case node_type_join:
            if (! join((join_node_t*) node, wme))
                continue;

            break;

        case node_type_instantiation:
            instantiation((instantiation_node_t*) node, wme);

            /* instantiation nodes are leaves, so they won't have any
               children.. */
            continue;
        }

        if (node->node.bindings)
            add_wme((inner_node_t*)node->children, node->node.bindings, wme);
    }
}

void
rete_add_wme(rete_t* rete, wme_t* wme)
{
    add_wme(rete->root, 0, wme);
}

void
rete_init(rete_t* rete, wme_t* wmes)
{
    wme_t* wme;
    for (wme = wmes; wme->object != 0; ++wme)
        rete_add_wme(rete, wme);
}
