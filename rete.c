#include "pool.h"
#include "wmem.h"
#include "rete.h"

struct binding_list {
    symbol_t                variable;
    struct variable_binding binding;
    struct binding_list*    next;
};

void
rete_init(struct rete* rete)
{
    rete->root.type = beta_node_type_root;
}

static inline struct beta_node*
new_beta_node(struct rete* net)
{
    return (struct beta_node*) pool_alloc(net->beta_node_pool);
}

static inline struct beta_test*
new_beta_test(struct rete* net)
{
    return (struct beta_test*) pool_alloc(net->beta_test_pool);
}

static const struct variable_binding*
find_bound_variable(const struct binding_list* bindings, const symbol_t* variable)
{
    while (bindings) {
        if (SYMBOLS_ARE_EQUAL(bindings->variable, *variable))
            return &bindings->binding;
    }

    return 0;
}

/*
 * Convert a `test' from a condition into the appropriate RETE
 * structures: constants that are tested by the alpha network and
 * nodes in the beta network.
 */
static void
process_test(struct rete* net,
             const struct test* test,
             unsigned field,
             const struct binding_list* bindings,
             symbol_t* constant)
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
            struct beta_test* beta_test = new_beta_test(net);
            beta_test->type  = test->type;
            beta_test->field = field;
            beta_test->data.variable_referent = *find_bound_variable(bindings, &test->data.referent);
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
                               struct beta_node* parent,
                               struct binding_list** bindings)
{
    symbol_t alpha_id, alpha_attr, alpha_value;
    process_test(net, &cond->data.simple.id_test,    field_id,    *bindings, &alpha_id);
    process_test(net, &cond->data.simple.attr_test,  field_attr,  *bindings, &alpha_attr);
    process_test(net, &cond->data.simple.value_test, field_value, *bindings, &alpha_value);
    return 0;
}


void
rete_add_production(struct rete* net, const struct production* p)
{
    struct beta_node* parent = &net->root;
    struct condition* cond;

    struct binding_list* bindings = 0;

    for (cond = p->lhs; cond != 0; cond = cond->next) {
        struct beta_node* child;

        switch (cond->type) {
        case condition_type_positive:
            child = create_positive_condition_node(net, cond, parent, &bindings);
            break;

        case condition_type_negative:
        case condition_type_conjunctive_negation:
            break;
        }

        assert(child != 0);
        parent = child;
    }

    while (bindings) {
        struct binding_list* doomed = bindings;
        bindings = bindings->next;
        pool_free(net->variable_binding_pool, doomed);
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
