#include "soar.h"
#include "pool.h"

static symbol_t
instantiate_rhs_value(struct rhs_value* value, struct token* token)
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
        assert(0); /* XXX writeme */
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

    struct action* action;

    inst->production = production;
    inst->token      = token;
    inst->next       = prefmem->instantiations;

    prefmem->instantiations = inst;

    for (action = production->rhs; action != 0; action = action->next) {
        struct preference* pref =
            (struct preference*) pool_alloc(&prefmem->preference_pool);

        pref->next = prefmem->preferences;
        prefmem->preferences = pref;

        pref->support = 0; /*XXX*/
        pref->type    = action->preference_type;

        pref->id    = instantiate_rhs_value(&action->id, token);
        pref->attr  = instantiate_rhs_value(&action->attr, token);
        pref->value = instantiate_rhs_value(&action->value, token);

        if (action->preference_type & preference_type_binary) {
            pref->referent = instantiate_rhs_value(&action->referent, token);
        }
    }
}

void
pref_init(struct prefmem* prefmem)
{
    pool_init(&prefmem->instantiation_pool, sizeof(struct instantiation), 8);
    pool_init(&prefmem->preference_pool,    sizeof(struct preference),    8);
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
