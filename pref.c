#include "soar.h"
#include "pool.h"

static symbol_t
instantiate_rhs_value(struct rhs_value* value,
                      struct token* token,
                      struct symbol_list* unbound_vars)
{
    symbol_t result;

    switch (value->type) {
    case rhs_value_type_symbol:
        result = value->val.symbol;
        break;

    case rhs_value_type_variable_binding:
        result = rete_get_variable_binding(value->val.variable_binding, token);
        break;
        
    case rhs_value_type_unbound_variable:
        {
            int index = (int) value->val.unbound_variable;
            while (--index > 0)
                unbound_vars = unbound_vars->next;

            result = unbound_vars->symbol;
        }
        break;
    }

    return result;
}

static void
create_instantiation(struct prefmem* prefmem,
                     const struct production* production,
                     struct token* token)
{
    struct instantiation* inst =
        (struct instantiation*) pool_alloc(&prefmem->instantiation_pool);

    struct symbol_list* unbound_vars = 0;
    struct action* action;
    int count;

    /* generate identifiers for the unbound variables */
    for (count = (int) production->num_unbound_vars - 1; count >= 0; --count) {
        struct symbol_list* entry =
            (struct symbol_list*) pool_alloc(&prefmem->symbol_list_pool);

        MAKE_SYMBOL(entry->symbol, symbol_type_identifier, /*XXX*/ 4);

        entry->next = unbound_vars;
        unbound_vars = entry;
    }

    inst->production = production;
    inst->token      = token;
    inst->next       = prefmem->instantiations;

    prefmem->instantiations = inst;

    for (action = production->actions; action != 0; action = action->next) {
        struct preference* pref =
            (struct preference*) pool_alloc(&prefmem->preference_pool);

        pref->next = prefmem->preferences;
        prefmem->preferences = pref;

        pref->support = 0; /*XXX*/
        pref->type    = action->preference_type;

        pref->id    = instantiate_rhs_value(&action->id, token, unbound_vars);
        pref->attr  = instantiate_rhs_value(&action->attr, token, unbound_vars);
        pref->value = instantiate_rhs_value(&action->value, token, unbound_vars);

        if (action->preference_type & preference_type_binary) {
            pref->referent = instantiate_rhs_value(&action->referent, token, unbound_vars);
        }
    }

    /* release unbound variables */
    while (unbound_vars) {
        struct symbol_list* doomed = unbound_vars;
        unbound_vars = unbound_vars->next;
        pool_free(&prefmem->symbol_list_pool, doomed);
    }
}

void
pref_init(struct prefmem* prefmem)
{
    pool_init(&prefmem->instantiation_pool, sizeof(struct instantiation), 8);
    pool_init(&prefmem->preference_pool,    sizeof(struct preference),    8);
    pool_init(&prefmem->symbol_list_pool,   sizeof(struct symbol_list),   8);
    prefmem->instantiations = 0;
}

void
pref_process_matches(struct prefmem* prefmem,
                     struct match* assertions,
                     struct match* retractions)
{
    for ( ; assertions != 0; assertions = assertions->next)
        create_instantiation(prefmem, assertions->production, assertions->token);

    for ( ; retractions != 0; retractions = retractions->next)
        /* retract_instantiation(...) */;
}
