#include "soar.h"
#include "alloc.h"

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
        /* If the rhs_value is a symbol, just return the symbol */
        result = value->val.symbol;
        break;

    case rhs_value_type_variable_binding:
        /* If the rhs_value is a variable binding, then use the token
           to compute the symbol that's bound. */
        result = rete_get_variable_binding(value->val.variable_binding, token);
        break;
        
    case rhs_value_type_unbound_variable:
        /* If the rhs_value is an unbound variable, grovel through the
           instantiated unbound variables to find an identifier. */
        {
            int index = (int) value->val.unbound_variable;
            while (--index > 0)
                unbound_vars = unbound_vars->next;

            result = unbound_vars->symbol;
        }
        break;
    }

    /* Sanity check the result */
    ASSERT(result.type != symbol_type_variable, ("rhs_value bound to variable"));

    return result;
}

/*
 * Instantiate a production.
 */
static void
create_instantiation(struct agent* agent,
                     struct production* production,
                     struct token* token,
                     struct instantiation** instantiations)
{
    struct instantiation* inst =
        (struct instantiation*) malloc(sizeof(struct instantiation));

    struct symbol_list* unbound_vars = 0;
    struct action* action;
    int count;

    /* generate identifiers for the unbound variables */
    for (count = (int) production->num_unbound_vars - 1; count >= 0; --count) {
        struct symbol_list* entry =
            (struct symbol_list*) malloc(sizeof(struct symbol_list));

        entry->symbol = agent_get_identifier(agent);
        entry->next   = unbound_vars;
        unbound_vars  = entry;
    }

    /* initialize the instantiation */
    inst->next        = *instantiations;
    inst->production  = production;
    inst->token       = token;
    inst->preferences = 0;

    *instantiations = inst;

    /* process the right-hand side of the production */
    for (action = production->actions; action != 0; action = action->next) {
        struct preference* pref =
            (struct preference*) malloc(sizeof(struct preference));

        pref->next_in_slot = 0;

        pref->next_in_instantiation = inst->preferences;
        inst->preferences = pref;

        pref->support = 0; /*XXX*/
        pref->type    = action->preference_type;

        pref->id    = instantiate_rhs_value(&action->id, token, unbound_vars);
        pref->attr  = instantiate_rhs_value(&action->attr, token, unbound_vars);
        pref->value = instantiate_rhs_value(&action->value, token, unbound_vars);

        if (action->preference_type & preference_type_binary) {
            ASSERT(SYMBOLS_ARE_EQUAL(pref->attr, agent->operator_symbol),
                   ("binary preference on non-operator"));

            pref->referent = instantiate_rhs_value(&action->referent, token, unbound_vars);
        }
    }

    /* release unbound variables */
    while (unbound_vars) {
        struct symbol_list* doomed = unbound_vars;
        unbound_vars = unbound_vars->next;
        free(doomed);
    }
}

/*
 * Process new matches, adding instantiations for `assertions', and
 * retracting them for `retractions'.
 */
void
pref_process_matches(struct agent* agent)
{
    struct instantiation* inst = 0;
    struct match* match;
    struct match* doomed;

    /* create instantiations for assertions */
    match = agent->assertions;
    while (match) {
        create_instantiation(agent, match->production, match->token, &inst);

        doomed = match;
        match = match->next;
        free(doomed);
    }

    /* The assertions have now been processed */
    agent->assertions = 0;

    /* process each instantiation, adding new preferences to temporary
       memory */
    while (inst) {
        struct instantiation* next = inst->next;
        struct preference* pref;

        /* transfer instantiation to the production that `owns' it */
        inst->next = inst->production->instantiations;
        inst->production->instantiations = inst;

        for (pref = inst->preferences; pref != 0; pref = pref->next_in_instantiation)
            wmem_add_preference(agent, pref);

        /* on to the next one */
        inst = next;
    }

    match = agent->retractions;
    while (match) {
        /* retract_instantiation(...); */

        doomed = match;
        match = match->next;
        free(doomed);
    }

    /* The retractions have now been processed */
    agent->retractions = 0;
}


struct preference*
pref_create_preference(symbol_t id, symbol_t attr, symbol_t value,
                       preference_type_t type,
                       support_type_t support)
{
    struct preference* pref =
        (struct preference*) malloc(sizeof(struct preference));

    pref->next_in_slot = pref->next_in_instantiation = 0;
    pref->type    = type;
    pref->support = support;
    pref->id      = id;
    pref->attr    = attr;
    pref->value   = value;

    return pref;
}
