#include "tcl.h"
#include "soar.h"
#include "symtab.h"
#include "parser.h"

static struct agent agent;
static struct symtab symtab;

static int
sp_command(ClientData data, Tcl_Interp* interp, int argc, char* argv[])
{
    struct production* prod;

    if (argc < 2) {
        interp->result = "too few arguments";
        return TCL_ERROR;
    }
    else if (argc > 2) {
        interp->result = "too many arguments";
        return TCL_ERROR;
    }

    prod = soar_parse_rule(&symtab, argv[1]);
    if (! prod) {
        interp->result = "error parsing rule";
        return TCL_ERROR;
    }

    rete_add_production(&agent, prod);

    return TCL_OK;
}

#if defined(_WIN32)
__declspec(dllexport)
#endif
int
Tinysoar_Init(Tcl_Interp* interp)
{
    Tcl_PkgProvide(interp, "TinySoar", "1.0");

    agent_init(&agent);
    symtab_init(&symtab);
    Tcl_CreateCommand(interp, "sp", sp_command, 0, 0);

    return TCL_OK;
}
