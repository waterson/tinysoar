#include "soar.h"
#include "pool.h"

void
symtab_init(struct agent* agent)
{
    agent->next_available_identifier = 1;
}

symbol_t
symtab_get_identifier(struct agent* agent)
{
    symbol_t result;

    /* XXX once we run out of identifiers, we need to `gc' working
       memory to identify ranges of free identifiers from which to
       allocate ids. */
    ASSERT(agent->next_available_identifier < (1 << SYMBOL_VALUE_BITS),
           ("ran out of identifiers"));

    MAKE_SYMBOL(result, symbol_type_identifier, agent->next_available_identifier++);
    return result;
}
