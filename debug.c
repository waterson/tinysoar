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
 * Debugging support.
 */
#ifdef DEBUG

#include <stdarg.h>
#include <string.h>
#include "config.h"
#include "soar.h"
#include "symtab.h"

#include <stdio.h>

/*
 * Alternative to assert() that traps in the debugger with a pretty
 * message.
 */
void
runtime_assert(const char *fmtstr, ...)
{
    va_list ap;

    va_start(ap, fmtstr);
    vfprintf(stderr, fmtstr, ap);
    va_end(ap);

    if (fmtstr[strlen(fmtstr) - 1] != '\n')
        fprintf(stderr, "\n");

#if defined(_MSC_VER) && _M_IX86 >= 300
    __asm { int 3 };
#elif defined(__GNUC__) && defined(__i386)
    asm("int $3");
#else
    abort();
#endif
}

void
runtime_warn(const char *fmtstr, ...)
{
    va_list ap;

    va_start(ap, fmtstr);
    vfprintf(stdout, fmtstr, ap);
    va_end(ap);

    if (fmtstr[strlen(fmtstr) - 1] != '\n')
        fprintf(stdout, "\n");
}

void
debug_indent_by(int nest)
{
    int i;
    for (i = 0; i < nest; ++i)
        printf("  ");
}

const char *
debug_test_type_to_string(test_type_t type)
{
    switch (type) {
    case test_type_blank:            return "(nil)";
    case test_type_equality:         return "==";
    case test_type_not_equal:        return "<>";
    case test_type_less:             return "<";
    case test_type_greater:          return ">";
    case test_type_less_or_equal:    return "<=";
    case test_type_greater_or_equal: return ">=";
    case test_type_same_type:        return "<=>";
    case test_type_conjunctive:      return "&&";
    case test_type_disjunctive:      return "||";
    case test_type_goal_id:          return "(goal)";
    }

    UNREACHABLE();
    return "(error!)";
}

const char *
debug_symbol_to_string(struct symtab *symtab, symbol_t symbol)
{
    static char buf[16];

    if (SYMBOL_IS_NIL(symbol))
        return "*";

    switch (GET_SYMBOL_TYPE(symbol)) {
    case symbol_type_symbolic_constant:
        if (symtab)
            return symtab_find_name(symtab, symbol);

        sprintf(buf, "(%d)", GET_SYMBOL_VALUE(symbol));
        break;

    case symbol_type_identifier:
        sprintf(buf, "[%d]", GET_SYMBOL_VALUE(symbol));
        break;

    case symbol_type_integer_constant:
        sprintf(buf, "%d", GET_SYMBOL_VALUE(symbol));
        break;

    case symbol_type_variable:
        sprintf(buf, "<%d>", GET_SYMBOL_VALUE(symbol));
        break;
    }

    /* XXX Leak, but this is debug-only code, so who cares. */
    return strdup(buf);
}

void
debug_dump_wme(struct symtab *symtab, struct wme *wme)
{
    printf("wme@%p(%s ", wme, debug_symbol_to_string(symtab, wme->slot->id));
    printf("^%s ", debug_symbol_to_string(symtab, wme->slot->attr));
    printf("%s", debug_symbol_to_string(symtab, wme->value));
    if (wme->type == wme_type_acceptable)
        printf(" +");
    printf(")");
}

void
debug_dump_token(struct symtab *symtab, struct token *token)
{
    printf("token@%p<parent=%p wme=", token, token->parent);
    if (token->wme)
        debug_dump_wme(symtab, token->wme);
    else
        printf("(null)");
    if (token->shared)
        printf(" shared");
    printf(">");
}

void
debug_dump_variable_binding(variable_binding_t binding)
{
    printf("<%d,", GET_VARIABLE_BINDING_DEPTH(binding));
    switch (GET_VARIABLE_BINDING_FIELD(binding)) {
    case field_id:    printf("id");     break;
    case field_attr:  printf("attr");   break;
    case field_value: printf("value");  break;
    default:
        ERROR(("unexpected field"));
    }
    printf(">");
}

void
debug_dump_test(struct symtab *symtab, struct beta_test *test)
{
    switch (GET_BETA_TEST_TYPE(test)) {
    case test_type_conjunctive:
    case test_type_disjunctive:
        break;

    default:
        switch (GET_BETA_TEST_FIELD(test)) {
        case field_id:      printf("id");     break;
        case field_attr:    printf("attr");   break;
        case field_value:   printf("value");  break;
        default:
            ERROR(("unexpected field"));
        }
    }

    switch (GET_BETA_TEST_TYPE(test)) {
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
            struct beta_test *disjunct = test->data.disjuncts;
            printf("(");
            while (disjunct) {
                debug_dump_test(symtab, disjunct);

                disjunct = disjunct->next;

                if (disjunct)
                    printf(" || ");
            }
            printf(")");
        }
        return;

    case test_type_conjunctive:
        /* shouldn't ever hit this; conjunctive tests are
           converted into a list of single tests. */
        ERROR(("unexpected test"));
        break;

    case test_type_goal_id:
        printf("(G)");
        return;
    }

    if (GET_BETA_TEST_RELATIONAL_TYPE(test) == relational_type_constant) {
        printf("%s", debug_symbol_to_string(symtab, test->data.constant_referent));
    }
    else {
        debug_dump_variable_binding(test->data.variable_referent);
    }
}

const char *
debug_preference_type_to_string(preference_type_t type)
{
    switch (type) {
    case preference_type_acceptable:         return "+";
    case preference_type_reject:             return "-";
    case preference_type_reconsider:         return "@";
    case preference_type_unary_indifferent:  return "=";
    case preference_type_best:               return ">";
    case preference_type_worst:              return "<";
    case preference_type_prohibit:           return "~";
    case preference_type_require:            return "!";
    case preference_type_binary_indifferent: return "=";
    case preference_type_better:             return ">";
    case preference_type_worse:              return "<";
    default:
        break;
    }

    UNREACHABLE();
    return 0;
}

void
debug_dump_rhs_value(struct symtab *symtab, struct rhs_value *value)
{
    switch (value->type) {
    case rhs_value_type_symbol:
        printf("%s", debug_symbol_to_string(symtab, value->val.symbol));
        break;

    case rhs_value_type_variable_binding:
        debug_dump_variable_binding(value->val.variable_binding);
        break;

    case rhs_value_type_unbound_variable:
        printf("?%d", value->val.unbound_variable);
        break;

    default:
        UNREACHABLE();
    }
}

void
debug_dump_beta_node(struct symtab    *symtab,
                     struct beta_node *node,
                     int               nest,
                     int               recur,
                     int               tokens)
{
    debug_indent_by(nest);

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
            struct beta_test *test;
            printf(" tests={");
            for (test = node->data.tests; test != 0; test = test->next) {
                printf(" ");
                debug_dump_test(symtab, test);
            }
            printf(" }");
        }
        break;

    case beta_node_type_production:
        printf(" name=%s", node->data.production->name);
        break;

    default:
        break;
    }

    printf(">");

    if (node->alpha_node) {
        printf(" (%s ^%s %s)",
               debug_symbol_to_string(symtab, node->alpha_node->id),
               debug_symbol_to_string(symtab, node->alpha_node->attr),
               debug_symbol_to_string(symtab, node->alpha_node->value));
    }

    printf("\n");

    if (node->type == beta_node_type_production) {
        /* Dump the RHS actions. */
        struct action *action = node->data.production->actions;
        for ( ; action != 0; action = action->next) {
            debug_indent_by(nest + 2);
            printf("=> (");
            debug_dump_rhs_value(symtab, &action->id);
            printf(" ^");
            debug_dump_rhs_value(symtab, &action->attr);
            printf(" ");
            debug_dump_rhs_value(symtab, &action->value);
            printf(" %s", debug_preference_type_to_string(action->preference_type));
            if (action->preference_type & preference_type_binary) {
                printf(" ");
                debug_dump_rhs_value(symtab, &action->referent);
            }
            printf(")\n");
        }
    }

    if (recur) {
        switch (node->type) {
        case beta_node_type_root:
        case beta_node_type_memory:
        case beta_node_type_memory_positive_join:
        case beta_node_type_negative:
        case beta_node_type_production:
            if (tokens) {
                /* dump tokens at the node */
                struct token *token;
                for (token = node->tokens; token != 0; token = token->next) {
                    debug_indent_by(nest + 2);
                    printf("+ ");
                    debug_dump_token(symtab, token);
                    printf("\n");
                }
            }

        default:
            break;
        }

        switch (node->type) {
        case beta_node_type_negative:
            if (tokens) {
                /* dump blocked tokens at the node */
                struct token *token;
                for (token = node->blocked; token != 0; token = token->next) {
                    debug_indent_by(nest + 2);
                    printf("x ");
                    debug_dump_token(symtab, token);
                    printf("\n");
                }
            }

        default:
            break;
        }

        for (node = node->children; node != 0; node = node->siblings)
            debug_dump_beta_node(symtab, node, nest + 1, 1, tokens);
    }
}

void
debug_dump_alpha_node(struct symtab *symtab, struct alpha_node *alpha, bool_t acceptable)
{
    struct beta_node *beta;
    struct right_memory *rm;

    printf("(%s ", debug_symbol_to_string(symtab, alpha->id));
    printf("^%s ", debug_symbol_to_string(symtab, alpha->attr));
    printf("%s", debug_symbol_to_string(symtab, alpha->value));
    if (acceptable)
        printf(" +");
    printf(")\n");

    for (rm = alpha->right_memories; rm != 0; rm = rm->next_in_alpha_node) {
        debug_indent_by(2);
        printf("+ ");
        debug_dump_wme(symtab, rm->wme);
        printf("\n");
    }

    for (beta = alpha->children; beta != 0; beta = beta->next_with_same_alpha_node)
        debug_dump_beta_node(symtab, beta, 1, 0, 1);

}

void
debug_dump_preference(struct symtab *symtab, struct preference *pref)
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

#ifdef DEBUG_GLIBC_MALLOC
/*
 * malloc and free wrappers that fill newly allocated and newly freed
 * memory. Useful for detecting usage of uninitialized or deleted
 * objects.
 */
extern __ptr_t __libc_malloc(size_t);
extern void __libc_free(__ptr_t);

static int live_blocks = 0;

__ptr_t
malloc(size_t sz)
{
    __ptr_t p = __libc_malloc(sz);
    if (p)
        memset(p, 0xcd, sz);

    ++live_blocks;
    return p;
}

void
free(__ptr_t p)
{
    if (p) {
        /* In glibc-2.2.4, malloc uses a two word block header: the
           second word -- modulo the low bit -- is the size of the
           current block. */
        size_t sz = *(((size_t *) p) - 1) & ~0x1;
        memset(p, 0xdd, sz - 4 /* XXX shrug! */);
    }

    --live_blocks;
    __libc_free(p);
}
#endif

#endif /* DEBUG */

