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

static void
indent_by(int nest)
{
    int i;
    for (i = 0; i < nest; ++i)
        printf("  ");
}

static const char*
symbol_to_string(symbol_t symbol)
{
    static char buf[16];

    if (SYMBOL_IS_NIL(symbol))
        return "*";

    switch (symbol.type) {
    case symbol_type_symbolic_constant:
        return symtab_find_name(&symtab, symbol);

    case symbol_type_identifier:
        sprintf(buf, "[%d]", symbol.val);
        break;

    case symbol_type_integer_constant:
        sprintf(buf, "%d", symbol.val);
        break;

    case symbol_type_variable:
        sprintf(buf, "<%d>", symbol.val);
        break;
    }

    return buf;
}

static void
dump_wme(struct wme* wme)
{
    printf("wme@%p(%s ", wme, symbol_to_string(wme->slot->id));
    printf("^%s ", symbol_to_string(wme->slot->attr));
    printf("%s)", symbol_to_string(wme->value));
}

static void
dump_token(struct token* token)
{
    printf("token@%p<parent=%p wme=", token, token->parent);
    if (token->wme)
        dump_wme(token->wme);
    else
        printf("(null)");
    printf(">");
}

static void
dump_test(struct beta_test* test)
{
    switch (test->type) {
    case test_type_blank:
        printf("0");
        break;

    case test_type_equality:
        printf("==");
        break;

    case test_type_not_equal:
        printf("!=");
        break;

    case test_type_less:
        printf("<");
        break;

    case test_type_greater:
        printf(">");
        break;

    case test_type_less_or_equal:
        printf("<=");
        break;

    case test_type_greater_or_equal:
        printf(">=");
        break;

    case test_type_same_type:
        printf("<=>");
        break;

    case test_type_disjunctive:
        {
            struct beta_test* disjunct = test->data.disjuncts;
            while (disjunct) {
                printf("( ");
                dump_test(disjunct);
                printf(")");

                disjunct = disjunct->next;

                if (disjunct)
                    printf(" || ");
            }
        }
        return;

    case test_type_conjunctive:
        /* shouldn't ever hit this; conjunctive tests are
           converted into a list of single tests. */
        ERROR(("unexpected test"));
        break;

    case test_type_goal_id:
        printf("G");
        return;

    case test_type_impasse_id:
        printf("I");
        return;
    }

    if (test->relational_type == relational_type_constant) {
        printf("%s", symbol_to_string(test->data.constant_referent));
    }
    else {
        printf("<%d,", test->data.variable_referent.depth);
        switch (test->data.variable_referent.field) {
        case field_id:      printf("id");     break;
        case field_attr:    printf("attr");   break;
        case field_value:   printf("value");  break;
        default:
            break;
        }
        printf(">");
    }
}

static void
dump_beta_node(struct beta_node* node, int nest, int recur)
{
    indent_by(nest);

    printf("beta-node@%p", node);

    switch (node->type) {
    case beta_node_type_memory:
        printf("(m)");
        break;

    case beta_node_type_positive_join:
        printf("(pos)");
        break;

    case beta_node_type_memory_positive_join:
        printf("(mpos)");
        break;

    case beta_node_type_negative:
        printf("(neg)");
        break;

    case beta_node_type_root:
        printf("(root)");
        break;

    case beta_node_type_conjunctive_negative:
        printf("(cneg)");
        break;

    case beta_node_type_conjunctive_negative_partner:
        printf("(cnp)");
        break;

    case beta_node_type_production:
        printf("(prod)");
        break;
    }

    printf("<parent=%p", node->parent);

    switch (node->type) {
    case beta_node_type_positive_join:
    case beta_node_type_memory_positive_join:
    case beta_node_type_negative:
        {
            struct beta_test* test;
            printf(" tests={");
            for (test = node->data.tests; test != 0; test = test->next) {
                printf(" ");
                dump_test(test);
            }
            printf(" }");
        }
        break;

    case beta_node_type_production:
#ifdef DEBUG
        printf(" name=%s", node->data.production->name);
#endif
        break;
    }

    printf(">\n");

    if (recur) {
        switch (node->type) {
        case beta_node_type_memory:
        case beta_node_type_memory_positive_join:
        case beta_node_type_negative:
        case beta_node_type_production:
            {
                /* dump tokens at the node */
                struct token* token;
                for (token = node->tokens; token != 0; token = token->next) {
                    indent_by(nest + 2);
                    printf("+ ");
                    dump_token(token);
                    printf("\n");
                }
            }

        default:
            break;
        }

        switch (node->type) {
        case beta_node_type_negative:
            {
                /* dump blocked tokens at the node */
                struct token* token;
                for (token = node->blocked; token != 0; token = token->next) {
                    indent_by(nest + 2);
                    printf("x ");
                    dump_token(token);
                    printf("\n");
                }
            }

        default:
            break;
        }

        for (node = node->children; node != 0; node = node->siblings)
            dump_beta_node(node, nest + 1, 1);
    }
}

/*
 * `dump-rete'.
 */
static int
dump_rete_command(ClientData data, Tcl_Interp* interp, int argc, char* argv[])
{
    int i;

    printf("\nALPHA NETWORK\n");
    for (i = 0; i < 16; ++i) {
        struct alpha_node* alpha = agent.alpha_nodes[i];
        while (alpha) {
            struct beta_node* beta;
            struct right_memory* rm;

            printf("%s ", symbol_to_string(alpha->id));
            printf("^%s ", symbol_to_string(alpha->attr));
            printf("%s\n", symbol_to_string(alpha->value));

            for (rm = alpha->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
                indent_by(2);
                printf("+ ");
                dump_wme(rm->wme);
                printf("\n");
            }

            for (beta = alpha->children; beta != 0; beta = beta->siblings)
                dump_beta_node(beta, 1, 0);

            alpha = alpha->siblings;
        }
    }

    printf("\nBETA NETWORK\n");
    dump_beta_node(&agent.root_node, 0, 1);
    printf("\n");

    return TCL_OK;
}

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

    Tcl_CreateCommand(interp, "dump-rete",   dump_rete_command,   0, 0);
    Tcl_CreateCommand(interp, "elaborate",   elaborate_command,   0, 0);
    Tcl_CreateCommand(interp, "init-soar",   init_soar_command,   0, 0);
    Tcl_CreateCommand(interp, "preferences", preferences_command, 0, 0);
    Tcl_CreateCommand(interp, "print",       print_command,       0, 0);
    Tcl_CreateCommand(interp, "sp",          sp_command,          0, 0);

    return TCL_OK;
}
