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
 * `preferences'. Query or add preferences to working memory.
 */
static int
preferences_command(ClientData data, Tcl_Interp* interp, int argc, char* argv[])
{
static char result[16];

    symbol_t id, attr, value;
    int i;
    int op = 0; /* query */
    preference_type_t type;
    struct preference* pref;

    i = 1;

    if (argc < 2) {
        interp->result = "preferences [-a] <id> ^<attr> [<value> [<pref> [<ref>]]]\n\
preferences -r <pref>";
        return TCL_ERROR;
    }

    if (strncmp(argv[i], "-a", 2) == 0) {
        op = 1; /* add */
        ++i;
    }
    else if (strncmp(argv[i], "-r", 2) == 0) {
        op = 2; /* remove */
        ++i;
    }

    if (i >= argc) {
        interp->result = (op == 2) ? "expected preference" : "expected identifier";
        return TCL_ERROR;
    }

    if (op == 2) {
        /* handle remove */

        /* XXX yeah, baby */
        pref = (struct preference*) atoi(argv[i]);

        /* XXX it'd be nice to make sure there is a really a pref with
           said address before whacking it */
        wmem_remove_preference(&agent, pref);
        return TCL_OK;
    }

    MAKE_SYMBOL(id, symbol_type_identifier, atoi(argv[i++]));

    if (i >= argc || argv[i][0] != '^') {
        interp->result = "expected attribute";
        return TCL_ERROR;
    }

    attr = symtab_lookup(&symtab, symbol_type_symbolic_constant, argv[i++] + 1, (bool_t)(op == 1));
    if (SYMBOL_IS_NIL(attr))
        return TCL_OK;

    if (op == 0) {
        /* handle query */
        pref = wmem_get_preferences(&agent, id, attr);
        while (pref) {
            printf("%u: ", (unsigned) pref);

            switch (pref->value.type) {
            case symbol_type_symbolic_constant:
                printf("%s ", symtab_find_name(&symtab, pref->value));
                break;
            
            case symbol_type_integer_constant:
            case symbol_type_identifier:
                printf("%d ", pref->value.val);
                break;

            default:
                ERROR(("illegal value in preference"));
            }

            switch (pref->type) {
            case preference_type_acceptable:         printf("+"); break;
            case preference_type_reject:             printf("-"); break;
            case preference_type_reconsider:         printf("@"); break;
            case preference_type_unary_indifferent:
            case preference_type_binary_indifferent: printf("="); break;
            case preference_type_best:
            case preference_type_better:             printf(">"); break;
            case preference_type_worst:
            case preference_type_worse:              printf("<"); break;
            case preference_type_prohibit:           printf("~"); break;
            case preference_type_require:            printf("!"); break;
            default:                                 printf("?"); break;
            }

            if (pref->type & preference_type_binary) {
                switch (pref->referent.type) {
                case symbol_type_symbolic_constant:
                    printf("%s ", symtab_find_name(&symtab, pref->referent));
                    break;
            
                case symbol_type_integer_constant:
                case symbol_type_identifier:
                    printf("%d ", pref->referent.val);
                    break;

                default:
                    ERROR(("illegal referent in preference"));
                }
            }

            if (pref->support == support_type_osupport)
                printf(" :O");

            printf("\n");
            pref = pref->next_in_slot;
        }

        return TCL_OK;
    }

    /* If we get here, we're adding a preference */
    if (i >= argc) {
        interp->result = "expected preference value";
        return TCL_ERROR;
    }

    /* parse the value */
    if (argv[i][0] >= '0' && argv[i][0] <= '9') {
        MAKE_SYMBOL(value, symbol_type_identifier, atoi(argv[i++]));
    }
    else if (argv[i][0] == '+' || argv[i][0] == '-') {
        MAKE_SYMBOL(value, symbol_type_integer_constant, atoi(argv[i++]));
    }
    else {
        value = symtab_lookup(&symtab, symbol_type_symbolic_constant, argv[i++], (bool_t)(op == 1));
        if (SYMBOL_IS_NIL(value))
            return TCL_OK;
    }

    if (i >= argc) {
        interp->result = "expected preference type";
        return TCL_ERROR;
    }

    /* parse the type */
    switch (argv[i++][0]) {
    case '+': type = preference_type_acceptable;        break;
    case '-': type = preference_type_reject;            break;
    case '@': type = preference_type_reconsider;        break;
    case '=': type = preference_type_unary_indifferent; break;
    case '>': type = preference_type_best;              break;
    case '<': type = preference_type_worst;             break;
    case '~': type = preference_type_prohibit;          break;
    case '!': type = preference_type_require;           break;
    default:
        interp->result = "expected valid preference type";
        return TCL_ERROR;
    }

    if (i < argc) {
        /* XXX TODO: handle binary preferences */
        interp->result = "can't handle binary preferences yet";
        return TCL_ERROR;
    }

    /* do the nasty */
    pref = wmem_add_preference(&agent, id, attr, value, type, support_type_architecture);
    sprintf(result, "%u", (unsigned) pref);
    interp->result = result;

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
        interp->result = "print [-stack | <id>]";
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

    Tcl_CreateCommand(interp, "elaborate",   elaborate_command,   0, 0);
    Tcl_CreateCommand(interp, "init-soar",   init_soar_command,   0, 0);
    Tcl_CreateCommand(interp, "preferences", preferences_command, 0, 0);
    Tcl_CreateCommand(interp, "print",       print_command,       0, 0);
    Tcl_CreateCommand(interp, "sp",          sp_command,          0, 0);

    return TCL_OK;
}
