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
#include "rete.h"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

struct agent agent;
struct symtab symtab;

#ifndef max
static inline size_t
max(size_t a, size_t b)
{
    return (a > b) ? a : b;
}
#endif

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
            debug_dump_preference(&symtab, pref);
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

#ifdef DEBUG
/*
 * Find the pnode corresponding to the rule in the network.
 */
static struct beta_node *
find_rule(struct beta_node *node, const char *name)
{
    if (node->type == beta_node_type_production
        && strcmp(node->data.production->name, name) == 0) {
        return node;
    }

    for ( ; node != 0; node = node->siblings) {
        if (node->children) {
            struct beta_node *result =
                find_rule(node->children, name);

            if (result)
                return result;
        }
    }

    return 0;
}
#endif

/*
 * Given the specified `relative' variable binding, return the
 * variable symbol that is bound. A nil symbol (i.e., zero) is
 * returned if there is no binding.
 */
static symbol_t
find_variable_for(struct variable_binding_list *bindings,
                  variable_binding_t            binding,
                  int                           depth)
{
    int absolute_depth = depth - GET_VARIABLE_BINDING_DEPTH(binding);
    SET_VARIABLE_BINDING_DEPTH(binding, absolute_depth);
    for ( ; bindings != 0; bindings = bindings->next) {
        if (VARIABLE_BINDINGS_ARE_EQUAL(bindings->binding, binding))
            return bindings->variable;
    }

    return 0;
}

/*
 * Given the specified `relative' variable binding, return the
 * variable symbol that is bound. A new variable symbol is created and
 * assigned to the binding, if necessary.
 */
static symbol_t
ensure_variable_for(struct variable_binding_list **bindings,
                    variable_binding_t             binding,
                    int                            depth)
{
    symbol_t result;

    if (SYMBOL_IS_NIL(result = find_variable_for(*bindings, binding, depth))) {
        struct variable_binding_list *entry =
            (struct variable_binding_list *) malloc(sizeof(struct variable_binding_list));

        int absolute_depth = depth - GET_VARIABLE_BINDING_DEPTH(binding);
        SET_VARIABLE_BINDING_DEPTH(binding, absolute_depth);

        entry->binding = binding;
        INIT_SYMBOL(result, symbol_type_variable,
                    *bindings ? (GET_SYMBOL_VALUE((*bindings)->variable) + 1) : 1);
        entry->variable = result;
        entry->next = *bindings;
        *bindings = entry;
    }

    return result;
}

/*
 * Print the LHS tests associated with the specified beta node and
 * field.
 */
static void
print_tests(char                         **result,
            size_t                        *sz,
            struct agent                  *agent,
            struct variable_binding_list **bindings,
            int                            depth,
            struct beta_node              *node,
            field_t                        field)
{
    struct beta_test *test;
    symbol_t variable = 0;
    symbol_t constant = 0;
    bool_t goal = 0;
    bool_t acceptable = 0;

    /* Look for a constant binding in the alpha network. */
    if (node->alpha_node) {
        switch (field) {
        case field_id:    constant = node->alpha_node->id;    break;
        case field_attr:  constant = node->alpha_node->attr;  break;
        case field_value: constant = node->alpha_node->value; break;
        default:
            ERROR(("unexpected field type"));
        }

        /* Determine if the alpha node tests an acceptable preference. */
        if (field == field_value) {
            int i;

            for (i = 8; !acceptable && i < 16; ++i) {
                struct alpha_node *alpha;
                for (alpha = agent->alpha_nodes[i]; alpha != 0; alpha = alpha->siblings) {
                    if (alpha == node->alpha_node) {
                        acceptable = 1;
                        break;
                    }
                }
            }
        }
    }

    /* Look for a variable binding. */
    for (test = node->data.tests; test != 0; test = test->next) {
        if (test->field == field) {
            switch (test->type) {
            case test_type_goal_id: {
                /* XXX this is hokey. Why don't goal tests work like
                   other tests? */
                variable_binding_t binding;

                ASSERT(field == field_id, ("goal test on non-id field"));
                goal = 1;

                INIT_VARIABLE_BINDING(binding, field_id, 0); 
                variable = ensure_variable_for(bindings, binding, depth);
                break;
            }

            case test_type_conjunctive:
                ERROR(("unexpected test type"));
                /* Fall through... */
            case test_type_disjunctive:
                break;

            default:
                ASSERT(test->relational_type == relational_type_variable,
                       ("expected constant test to be in alpha node"));

                variable = ensure_variable_for(bindings, test->data.variable_referent, depth);
                break;
            }
        }
    }

    /* Now see if it's a disjunctive test. */
    for (test = node->data.tests; test != 0; test = test->next) {
        /* All the disjuncts will test the same field. */
        if (test->type == test_type_disjunctive && test->field == field) {
            struct beta_test *disjunct;

            vsmcatf(result, sz, "{");

            /* Degenerate, but possible. */
            if (goal)
                vsmcatf(result, sz, " state");

            vsmcatf(result, sz, " <<");

            if (variable)
                vsmcatf(result, sz, " <v%d>", GET_SYMBOL_VALUE(variable));

            for (disjunct = test->data.disjuncts; disjunct != 0; disjunct = disjunct->next) {
                ASSERT(disjunct->relational_type == relational_type_constant,
                       ("expected constant value in disjunct"));

                vsmcatf(result, sz, " %s",
                        symtab_find_name(&symtab,
                                         disjunct->data.constant_referent));
            }

            /* This would be really degenerate, but it's possible. */
            if (constant)
                vsmcatf(result, sz, " %s", symtab_find_name(&symtab, constant));

            vsmcatf(result, sz, " >> }");

            if (acceptable)
                vsmcatf(result, sz, " +");

            return;
        }
    }

    if (variable && constant) {
        vsmcatf(result, sz, "{ %s<v%d> %s }",
                (goal ? "state " : ""), /* Degenerate, but possible. */
                GET_SYMBOL_VALUE(variable),
                symtab_find_name(&symtab, constant));
    }
    else {
        if (goal)
            vsmcatf(result, sz, "state ");

        if (constant)
            vsmcatf(result, sz, "%s", symtab_find_name(&symtab, constant));
        else {
            if (!variable && field == field_value) {
                variable_binding_t binding;
                INIT_VARIABLE_BINDING(binding, field_value, 0);
                variable = ensure_variable_for(bindings, binding, depth);
            }

            vsmcatf(result, sz, "<v%d>", GET_SYMBOL_VALUE(variable));
        }
    }

    if (acceptable)
        vsmcatf(result, sz, " +");
}

/*
 * Print the specified RHS value.
 */
static void
print_rhs_value(char                         **result,
                size_t                        *sz,
                struct variable_binding_list  *bindings,
                int                            depth,
                struct rhs_value              *value)
{
    switch (value->type) {
    case rhs_value_type_symbol:
        vsmcatf(result, sz, "%s", symtab_find_name(&symtab, value->val.symbol));
        break;

    case rhs_value_type_variable_binding:
        {
            symbol_t var =
                find_variable_for(bindings, value->val.variable_binding, depth);

            vsmcatf(result, sz, "<v%d>", GET_SYMBOL_VALUE(var));
        }

        break;

    case rhs_value_type_unbound_variable:
        {
            int num = bindings ? GET_SYMBOL_VALUE(bindings->variable) : 0;
            vsmcatf(result, sz, "<v%d>", num + value->val.unbound_variable + 1);
        }
        break; 
    }
}

/*
 * Print the specified rule.
 */
struct beta_node_list {
    struct beta_node      *node;
    struct beta_node_list *next;
};

static bool_t
print_rule(char **result, size_t *sz, struct agent *agent, const char *name)
{
    struct beta_node *pnode = 0;

#ifdef DEBUG
    if (agent->root_node)
        pnode = find_rule(agent->root_node, name);
#endif

    if (pnode) {
        struct variable_binding_list *bindings = 0;
        struct beta_node_list *nodes;
        struct beta_node *node;
        struct action *action;
        int depth;

        vsmcatf(result, sz, "%s", name);

        /* Collect the nodes into a list so we can walk the list in
           reverse. (We could do this by reversing pointers, but I'm just
           not that desparate to make this space-efficient.) */
        nodes = 0;
        for (node = pnode; node != 0; node = node->parent) {
            struct beta_node_list *entry;

            switch (node->type) {
            case beta_node_type_positive_join:
            case beta_node_type_negative:
                entry = (struct beta_node_list *) malloc(sizeof(struct beta_node_list));
                entry->node = node;
                entry->next = nodes;
                nodes = entry;
                break;

            default:
                break;
            }
        }

        /* Print the rule's conditions. */
        depth = 0;
        while (nodes) {
            struct beta_node_list *next = nodes->next;

            vsmcatf(result, sz, "\n(");

            print_tests(result, sz, agent, &bindings, depth, nodes->node, field_id);

            /* XXX Hacky, but it works. */
            vsmcatf(result, sz, " %s^",
                    (nodes->node->type == beta_node_type_negative) ? "-" : "");

            print_tests(result, sz, agent, &bindings, depth, nodes->node, field_attr);

            vsmcatf(result, sz, " ");

            print_tests(result, sz, agent, &bindings, depth, nodes->node, field_value);

            vsmcatf(result, sz, ")");

            free(nodes);
            nodes = next;
            ++depth;
        }

        vsmcatf(result, sz, "\n-->");

        /* Print the rule's actions. */
        --depth;
        for (action = pnode->data.production->actions; action != 0; action = action->next) {
            vsmcatf(result, sz, "\n(");
            print_rhs_value(result, sz, bindings, depth, &action->id);
            vsmcatf(result, sz, " ^");
            print_rhs_value(result, sz, bindings, depth, &action->attr);
            vsmcatf(result, sz, " ");
            print_rhs_value(result, sz, bindings, depth, &action->value);

            /* XXX don't use `debug' function. */
            vsmcatf(result, sz, " %s", debug_preference_type_to_string(action->preference_type));
            if (action->preference_type & preference_type_binary) {
                vsmcatf(result, sz, " ");
                print_rhs_value(result, sz, bindings, depth, &action->referent);
            }

            vsmcatf(result, sz, ")");
        }

        /* Clean up the bound variables. */
        while (bindings) {
            struct variable_binding_list *doomed = bindings;
            bindings = bindings->next;
            free(doomed);
        }

        /* Let the caller know we found the rule. */
        return 1;
    }

    /* No such rule. */
    return 0;
}

/*
 * wme enumerator callback for `print': if this is the id we wanna print,
 * then this'll dump the dope.
 */
struct print_closure {
    char     **result;
    size_t    *sz;
    symbol_t   id;
};

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
        interp->result = "print [-stack | <id> | <rule>]";
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

    for ( ; i < argc; ++i) {
        int id = atoi(argv[i]);
        if (id) {
            if (print_result)
                vsmcatf(&print_result, &print_result_sz, "\n");

            INIT_SYMBOL(pc.id, symbol_type_identifier, atoi(argv[i]));
            vsmcatf(&print_result, &print_result_sz, "[%d]", GET_SYMBOL_VALUE(pc.id));
            wmem_enumerate_wmes(&agent, print_enumerator, &pc);
        }
        else {
            if (! print_rule(&print_result, &print_result_sz, &agent, argv[i])) {
                if (print_result) {
                    free(print_result);
                    print_result = 0;
                }

                vsmcatf(&print_result, &print_result_sz,
                        "no such rule, `%s'", argv[i]);

                interp->result = print_result;
                return TCL_ERROR;
            }
        }
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

    prod_add_production(&agent, prod);

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
    if (! Tcl_InitStubs(interp, TCL_VERSION, 1))
        return TCL_ERROR;

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

int
Tinysoar_SafeInit(Tcl_Interp *interp)
{
    return Tinysoar_Init(interp);
}

