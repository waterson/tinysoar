#include "pool.h"
#include "wmem.h"
#include "rete.h"

struct variable_binding_list {
    symbol_t                      variable;
    struct variable_binding       binding;
    struct variable_binding_list* next;
};

void
rete_init(struct rete* net)
{
    net->root.type = beta_node_type_root;
    pool_init(&net->beta_node_pool, sizeof(struct beta_node), 8);
    pool_init(&net->beta_test_pool, sizeof(struct beta_test), 8);
    pool_init(&net->variable_binding_list_pool, sizeof(struct variable_binding_list), 8);
}

static inline struct beta_node*
new_beta_node(struct rete* net)
{
    return (struct beta_node*) pool_alloc(&net->beta_node_pool);
}

static inline struct beta_test*
new_beta_test(struct rete* net)
{
    return (struct beta_test*) pool_alloc(&net->beta_test_pool);
}

static inline struct variable_binding_list*
new_variable_binding_list(struct rete* net)
{
    return (struct variable_binding_list*) pool_alloc(&net->variable_binding_list_pool);
}

static const struct variable_binding*
find_bound_variable(const struct variable_binding_list* bindings, const symbol_t* variable)
{
    while (bindings) {
        if (SYMBOLS_ARE_EQUAL(bindings->variable, *variable))
            return &bindings->binding;
    }

    return 0;
}

/*
 */
static void
bind_variables(struct rete* net,
               const struct test* test,
               unsigned depth,
               field_t field,
               struct variable_binding_list** bindings)
{
    switch (test->type) {
    case test_type_equality:
        if ((GET_SYMBOL_TYPE(test->data.referent) == symbol_type_variable) &&
            !find_bound_variable(*bindings, &test->data.referent)) {
            struct variable_binding_list* entry = new_variable_binding_list(net);
            entry->variable      = test->data.referent;
            entry->binding.depth = depth;
            entry->binding.field = field;
            entry->next = *bindings;
            *bindings = entry;
        }
        break;

    case test_type_conjunctive:
        break;

    default:
        /* do nothing */
        break;
    }
}

/*
 * Convert a `test' from a condition into the appropriate RETE
 * structures: constants that are tested by the alpha network and
 * nodes in the beta network.
 */
static void
process_test(struct rete* net,
             const struct test* test,
             field_t field,
             const struct variable_binding_list* bindings,
             symbol_t* constant,
             struct beta_test** tests)
{
    struct beta_test* beta_test = 0;

    if (test->type == test_type_blank)
        return;

    switch (test->type) {
    case test_type_equality:
        if (GET_SYMBOL_TYPE(test->data.referent) == symbol_type_variable) {
            /* It's a variable. Make a variable equality test */
            const struct variable_binding* binding;
            beta_test = new_beta_test(net);

            binding = find_bound_variable(bindings, &test->data.referent);
            assert(binding != 0);

            beta_test->data.variable_referent = *binding;
        }
        else {
            /* It's a constant. Install an alpha test if possible;
               otherwise, create a beta test node */
            if (! GET_SYMBOL_VALUE(*constant)) {
                *constant = test->data.referent;
            }
            else {
                beta_test = new_beta_test(net);
                beta_test->data.constant_referent = *constant;
            }
        }
        break;

    case test_type_goal_id:
        beta_test = new_beta_test(net);
        break;

    default:
        assert(0); /* XXX */
    }

    if (beta_test) {
        beta_test->type  = test->type;
        beta_test->field = field;
        beta_test->next = *tests;
        *tests = beta_test;
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
                               unsigned depth,
                               struct beta_node* parent,
                               struct variable_binding_list** bindings)
{
    struct beta_node* result;
    struct beta_test* tests = 0;
    symbol_t alpha_id, alpha_attr, alpha_value;

    CLEAR_SYMBOL(alpha_id);
    CLEAR_SYMBOL(alpha_attr);
    CLEAR_SYMBOL(alpha_value);

    bind_variables(net, &cond->data.simple.id_test,    depth, field_id,    bindings);
    bind_variables(net, &cond->data.simple.attr_test,  depth, field_attr,  bindings);
    bind_variables(net, &cond->data.simple.value_test, depth, field_value, bindings);

    process_test(net, &cond->data.simple.id_test,    field_id,    *bindings, &alpha_id,    &tests);
    process_test(net, &cond->data.simple.attr_test,  field_attr,  *bindings, &alpha_attr,  &tests);
    process_test(net, &cond->data.simple.value_test, field_value, *bindings, &alpha_value, &tests);

    result = new_beta_node(net);
    result->type = beta_node_type_positive_join;
    result->parent = parent;
    result->siblings = parent->children;
    parent->children = result;
    result->children = 0;
    result->data.tests = tests;

    return result;
}


void
rete_add_production(struct rete* net, const struct production* p)
{
    unsigned depth = 0;
    struct beta_node* parent = &net->root;
    struct condition* cond;

    struct variable_binding_list* bindings = 0;

    for (cond = p->lhs; cond != 0; cond = cond->next) {
        struct beta_node* child;

        switch (cond->type) {
        case condition_type_positive:
            child = create_positive_condition_node(net, cond, ++depth, parent, &bindings);
            break;

        case condition_type_negative:
        case condition_type_conjunctive_negation:
            break;
        }

        assert(child != 0);
        parent = child;
    }

    while (bindings) {
        struct variable_binding_list* doomed = bindings;
        bindings = bindings->next;
        pool_free(&net->variable_binding_list_pool, doomed);
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
