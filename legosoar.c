#include "soar.h"

#define DECL_RETE_NETWORK
#include "agent.inc"

int
kmain()
{
    agent_init(&agent);

    while (1)
        agent_elaborate(&agent);
        
    return 0;
}
