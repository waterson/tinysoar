#include "soar.h"

void
agent_init(struct agent* agent)
{
    wmem_init(agent);
    symtab_init(agent);
    rete_init(agent);
    pref_init(agent);
    tmem_init(agent);
}
