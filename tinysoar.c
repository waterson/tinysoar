/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "MPL"); you may not use this file except in
 * compliance with the MPL.  You may obtain a copy of the MPL at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the MPL is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the MPL
 * for the specific language governing rights and limitations under the
 * MPL.
 *
 * The Initial Developer of this code under the MPL is Christopher
 * R. Waterson. Portions created by Christopher R. Waterson are
 * Copyright (C) 2000 Christopher R. Waterson. All Rights Reserved.
 *
 * Contributor(s):
 *   Christopher R. Waterson <waterson@maubi.net>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or 
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 */

/*
 * Tcl Interface.
 */

#include "tcl.h"
#include "soar.h"
#include "symtab.h"
#include "parser.h"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

struct agent agent;
struct symtab symtab;

static inline size_t
max(size_t a, size_t b)
{
    return (a > b) ? a : b;
}

/*
 * Varargs-style malloc-ing formatted concatenation.
 */
static int
vsmcatf(char **result, size_t *sz, const char *format, ...)
{
    char *buf = *result;
    size_t length, required, remaining;
    va_list ap;

    va_start(ap, format);

    /* If we've got no buffer, allocate a minimal one to start with. */
    if (! buf) {
        *sz = 16;
        buf = malloc(*sz);
        if (! buf)
            goto out_of_memory;

        buf[0] = 0;
        *result = buf;
    }

    /* Try to fit the formatted string in the existing buffer. */
    length = strlen(buf);
    remaining = *sz - (length + 1);
    required = vsnprintf(buf + length, remaining, format, ap);

    if (required >= remaining) {
        /* It didn't fit. Allocate a new buffer big enough to hold the
           formatted string, and try again. */
        char *old_buf = buf;

        *sz = max(required + length + 1, *sz * 2);
        buf = malloc(*sz);
        if (! buf)
            goto out_of_memory;

        strcpy(buf, old_buf);
        free(old_buf);

        vsprintf(buf + length, format, ap);

        *result = buf;
    }

    return required;

 out_of_memory:
    *result = 0;
    *sz = 0;
    return -1;
}

/*
 * `dump-rete'.
 */
static int
dump_rete_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
#ifdef DEBUG
    rete_dump(&agent, &symtab);
#endif

    return TCL_OK;
}

/*
 * `elaborate'. Run the agent one elaboration cycle.
 */
static int
elaborate_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
    agent_elaborate(&agent);
    return TCL_OK;
}

/*
 * `export'. Dump the RETE network as a set of C structs.
 */
static int
export_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
    extern void soar_export(FILE *file, struct agent *agent, struct symtab *symtab); /*XXX*/
    FILE *file = stdout;

    if (argc > 1)
        file = fopen(argv[1], "w");

    if (! file) {
        interp->result = "failed to open file";
        return TCL_ERROR;
    }

    soar_export(file, &agent, &symtab);
    if (file != stdout)
        fclose(file);

    return TCL_OK;
}

/*
 * `init-soar'. Re-initialize the agent.
 */
static int
init_soar_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
    agent_reset(&agent);
    return TCL_OK;
}

static void
dump_preference(struct symtab *symtab, struct preference *pref)
{
    printf("%u: ", (unsigned) pref);

    switch (GET_SYMBOL_TYPE(pref->value)) {
    case symbol_type_symbolic_constant:
        printf("%s ", symtab_find_name(symtab, pref->value));
        break;
            
    case symbol_type_integer_constant:
        printf("%d ", GET_SYMBOL_VALUE(pref->value));
        break;

    case symbol_type_identifier:
        printf("[%d] ", GET_SYMBOL_VALUE(pref->value));
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
        switch (GET_SYMBOL_TYPE(pref->referent)) {
        case symbol_type_symbolic_constant:
            printf("%s ", symtab_find_name(symtab, pref->referent));
            break;
            
        case symbol_type_identifier:
            printf("[%d] ", GET_SYMBOL_VALUE(pref->referent));
            break;

        case symbol_type_integer_constant:
            printf("%d ", GET_SYMBOL_VALUE(pref->referent));
            break;

        default:
            ERROR(("illegal referent in preference"));
        }
    }

    if (pref->support == support_type_osupport)
        printf(" :O");

    if (pref->instantiation && pref->instantiation->production)
        printf(" (%s)", pref->instantiation->production->name);
}

/*
 * `preferences'. Query or add preferences to working memory.
 */
static int
preferences_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
static char preferences_result[16];

    symbol_t id, attr, value;
    int i;
    int op = 0; /* query */
    preference_type_t type;
    struct preference *pref;

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
        /* Handle remove. */

        /* XXX Yeah, baby! */
        pref = (struct preference *) atoi(argv[i]);

        /* XXX It'd be nice to make sure there is a really a pref with
           said address before whacking it. */
        wmem_remove_preference(&agent, pref, 0);
        return TCL_OK;
    }

    INIT_SYMBOL(id, symbol_type_identifier, atoi(argv[i++]));

    if (i >= argc || argv[i][0] != '^') {
        interp->result = "expected attribute";
        return TCL_ERROR;
    }

    attr = symtab_lookup(&symtab, symbol_type_symbolic_constant, argv[i++] + 1, (bool_t)(op == 1));
    if (SYMBOL_IS_NIL(attr))
        return TCL_OK;

    if (op == 0) {
        /* Handle a query. */
        pref = wmem_get_preferences(&agent, id, attr);
        while (pref) {
            dump_preference(&symtab, pref);
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

    /* Parse the value. */
    if (argv[i][0] >= '0' && argv[i][0] <= '9') {
        INIT_SYMBOL(value, symbol_type_identifier, atoi(argv[i++]));
    }
    else if (argv[i][0] == '+' || argv[i][0] == '-') {
        INIT_SYMBOL(value, symbol_type_integer_constant, atoi(argv[i++]));
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

    /* Parse the type. */
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

    /* Do the nasty. */
    pref = wmem_add_preference(&agent, id, attr, value, type, support_type_architecture);
    snprintf(preferences_result, sizeof preferences_result, "%u", (unsigned) pref);
    interp->result = preferences_result;

    return TCL_OK;
}


struct print_closure {
    char     **result;
    size_t    *sz;
    symbol_t   id;
};

/*
 * wme enumerator callback for `print': if this is the id we wanna print,
 * then this'll dump the dope.
 */
static void
print_enumerator(struct agent *agent, struct wme *wme, void *closure)
{
    struct print_closure *pc = (struct print_closure *) closure;

    if (SYMBOLS_ARE_EQUAL(pc->id, wme->slot->id)) {
        vsmcatf(pc->result, pc->sz, " ^%s ", symtab_find_name(&symtab, wme->slot->attr));

        switch (GET_SYMBOL_TYPE(wme->value)) {
        case symbol_type_symbolic_constant:
            vsmcatf(pc->result, pc->sz, "%s", symtab_find_name(&symtab, wme->value));
            break;

        case symbol_type_identifier:
            vsmcatf(pc->result, pc->sz, "[%d]", GET_SYMBOL_VALUE(wme->value));
            break;

        case symbol_type_integer_constant:
            vsmcatf(pc->result, pc->sz, "%d", GET_SYMBOL_VALUE(wme->value));
            break;

        case symbol_type_variable:
            ERROR(("variable unexpected"));
        }

        if (wme->type == wme_type_acceptable)
            vsmcatf(pc->result, pc->sz, " +");
    }
}

/*
 * `print'. Print stuff that's hanging off an identifier
 */
static int
print_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
static char *print_result;

    struct print_closure pc;
    size_t print_result_sz;
    int i;

    /* Free the last result buffer, if there was one. This makes the
       command non-reentrant, but that's probably okay. */
    if (print_result) {
        free(print_result);
        print_result = 0;
    }

    /* Sanity check the command. */
    if (argc < 2) {
        interp->result = "print [-stack | <id>]";
        return TCL_ERROR;
    }

    i = 1;

    if (strcmp(argv[i], "-stack") == 0) {
        struct goal_stack *goal;
        for (goal = agent.goals; goal != 0; goal = goal->next) {
            vsmcatf(&print_result, &print_result_sz,
                    "[%d]", GET_SYMBOL_VALUE(goal->symbol));

            if (goal->next)
                vsmcatf(&print_result, &print_result_sz, " ");
        }

        ++i;
    }

    pc.result = &print_result;
    pc.sz = &print_result_sz;

    while (i < argc) {
        INIT_SYMBOL(pc.id, symbol_type_identifier, atoi(argv[i++]));

        vsmcatf(&print_result, &print_result_sz, "[%d]", GET_SYMBOL_VALUE(pc.id));
        wmem_enumerate_wmes(&agent, print_enumerator, &pc);
    }

    interp->result = print_result;
    return TCL_OK;
}

/*
 * `sp'. Add a production.
 */
static int
sp_command(ClientData data, Tcl_Interp *interp, int argc, char *argv[])
{
    struct production *prod;

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

#ifdef DEBUG
    printf("compiled: %s\n", prod->name);
#endif

    return TCL_OK;
}

#if defined(_WIN32)
__declspec(dllexport)
#endif
int
Tinysoar_Init(Tcl_Interp *interp)
{
    Tcl_PkgProvide(interp, "TinySoar", "1.0");

    agent_init(&agent);
    symtab_init(&symtab);

    Tcl_CreateCommand(interp, "dump-rete",   dump_rete_command,   0, 0);
    Tcl_CreateCommand(interp, "elaborate",   elaborate_command,   0, 0);
    Tcl_CreateCommand(interp, "export",      export_command,      0, 0);
    Tcl_CreateCommand(interp, "init-soar",   init_soar_command,   0, 0);
    Tcl_CreateCommand(interp, "preferences", preferences_command, 0, 0);
    Tcl_CreateCommand(interp, "print",       print_command,       0, 0);
    Tcl_CreateCommand(interp, "sp",          sp_command,          0, 0);

    return TCL_OK;
}
