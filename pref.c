#include "soar.h"
#include "pool.h"

/*
 * Given a right-hand side value (i.e., an `rhs_value'), a token from
 * the RETE network, and a list of unbound variables, compute the
 * symbol that is the `instantiated value' for the right-hand side
 * value.
 */
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
create_instantiation(struct agent* agent,
                     const struct production* production,
                     struct token* token)
{
    struct instantiation* inst =
        (struct instantiation*) pool_alloc(&agent->instantiation_pool);

    struct symbol_list* unbound_vars = 0;
    struct action* action;
    int count;

    /* generate identifiers for the unbound variables */
    for (count = (int) production->num_unbound_vars - 1; count >= 0; --count) {
        struct symbol_list* entry =
            (struct symbol_list*) pool_alloc(&agent->symbol_list_pool);

        entry->symbol = symtab_get_identifier(agent);
        entry->next   = unbound_vars;
        unbound_vars  = entry;
    }

    /* initialize the instantiation */
    inst->production = production;
    inst->token      = token;
    inst->next       = agent->instantiations;

    agent->instantiations = inst;

    /* process the right-hand side of the production */
    for (action = production->actions; action != 0; action = action->next) {
        struct preference* pref =
            (struct preference*) pool_alloc(&agent->preference_pool);

        pref->next = agent->preferences;
        agent->preferences = pref;

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
        pool_free(&agent->symbol_list_pool, doomed);
    }
}

void
pref_init(struct agent* agent)
{
    pool_init(&agent->instantiation_pool, sizeof(struct instantiation), 8);
    pool_init(&agent->preference_pool,    sizeof(struct preference),    8);
    pool_init(&agent->symbol_list_pool,   sizeof(struct symbol_list),   8);
    agent->instantiations = 0;
    agent->preferences    = 0;
}

void
pref_process_matches(struct agent* agent)
{
    struct match* match;

    for (match = agent->assertions; match != 0; match = match->next)
        create_instantiation(agent, match->production, match->token);

    for (match = agent->retractions; match != 0; match = match->next)
        /* retract_instantiation(...) */;
}
