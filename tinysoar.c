/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*

  Tcl Interface

  TODO

  . Implement a `preferences' command that'll both display preferences
    and let you whack 'em in (for testing and stuff).

*/

#include "tcl.h"
#include "soar.h"
#include "symtab.h"
#include "parser.h"
#include <stdlib.h>
#include <string.h>

static struct agent agent;
static struct symtab symtab;

/*
 * `elaborate'. Run the agent one elaboration cycle.
 */
static int
elaborate_command(ClientData data, Tcl_Interp* interp, int argc, char* argv[])
{
    agent_elaborate(&agent);
    return TCL_OK;
}

/*
 * `init-soar'. Re-initialize the agent.
 */
static int
init_soar_command(ClientData data, Tcl_Interp* interp, int argc, char* argv[])
{
    agent_reset(&agent);
    return TCL_OK;
}

/*
 * wme enumerator callback for `print': if this is the id we wanna print,
 * then this'll dump the dope.
 */
static void
print_enumerator(struct agent* agent, struct wme* wme, void* closure)
{
    symbol_t* id = (symbol_t*) closure;
    if (SYMBOLS_ARE_EQUAL(*id, wme->slot->id)) {
        printf(" ^%s ", symtab_find_name(&symtab, wme->slot->attr));

        switch (wme->value.type) {
        case symbol_type_symbolic_constant:
            printf("%s", symtab_find_name(&symtab, wme->value));
            break;

        case symbol_type_identifier:
            printf("[%d]", wme->value.val);
            break;

        case symbol_type_integer_constant:
            printf("%d", wme->value.val);
            break;

        case symbol_type_variable:
            ERROR(("variable unexpected"));
        }

        if (wme->type == wme_type_acceptable)
            printf(" +");
    }
}

/*
 * `print'. Print stuff that's hanging off an identifier
 */
static int
print_command(ClientData data, Tcl_Interp* interp, int argc, char* argv[])
{
    int i;

    if (argc < 2) {
        interp->result = "too few arguments";
        return TCL_ERROR;
    }

    i = 1;

    if (strcmp(argv[i], "-stack") == 0) {
        struct symbol_list* goal;
        int indent;
        for (goal = agent.goals, indent = 0; goal != 0; goal = goal->next, ++indent) {
            int j = indent;
            while (--j > 0)
                printf("  ");

            printf("[%d]\n", goal->symbol.val);
        }

        ++i;
    }

    while (i < argc) {
        symbol_t id;
        MAKE_SYMBOL(id, symbol_type_identifier, atoi(argv[i++]));

        printf("([%d]", id.val);
        wmem_enumerate_wmes(&agent, print_enumerator, &id);
        printf(")\n");
    }

    return TCL_OK;
}

/*
 * `sp'. Add a production.
 */
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

    Tcl_CreateCommand(interp, "elaborate", elaborate_command, 0, 0);
    Tcl_CreateCommand(interp, "init-soar", init_soar_command, 0, 0);
    Tcl_CreateCommand(interp, "print",     print_command,     0, 0);
    Tcl_CreateCommand(interp, "sp",        sp_command,        0, 0);

    return TCL_OK;
}
