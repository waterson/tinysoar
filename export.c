/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*

  Export the RETE Network as a set of C structures.

*/

#include "alloc.h"
#include "soar.h"
#include "ht.h"
#include "symtab.h"
#include <stdio.h>

/*
 * An ``index map'' is a hashtable that keeps a mapping from a pointer
 * to a unique, monotonically ascending numeric identifier.
 */
struct index_map {
    void* ptr;
    int   id;
};

static inline unsigned
hash_pointer(const void* p)
{
    return ((unsigned) p) >> 3;
}

static bool_t
compare_map_entries(const struct index_map* p1, const struct index_map* p2)
{
    return p1->ptr == p2->ptr;
}

static ht_enumerator_result_t
free_index_map(struct ht_entry_header* header, void* closure)
{
    return ht_enumerator_result_delete;
}

/*
 * Retrieve the identifier for the specified pointer.
 */
static int
get_index_map_id(struct ht* table, void* ptr)
{
    struct index_map key;
    struct ht_entry_header** entryp;
    struct index_map* map;

    key.ptr = ptr;
    entryp = ht_lookup(table, hash_pointer(ptr), &key);

    if (! *entryp) {
        ERROR(("unindexed pointer %p", ptr));
        return -1;
    }

    map = (struct index_map*) HT_ENTRY_DATA(*entryp);
    return map->id;
}

/*
 * Associate an identifier with the specified pointer.
 */
static void
put_index_map_id(struct ht* table, void* ptr, int id)
{
    struct index_map key;
    unsigned hash = hash_pointer(ptr);
    struct ht_entry_header** entryp;
    struct ht_entry_header* entry;
    struct index_map* map;

    key.ptr = ptr;
    entryp = ht_lookup(table, hash, &key);

    ASSERT(! *entryp, ("already added %p", ptr));

    entry = (struct ht_entry_header*) malloc(sizeof(struct ht_entry_header) + sizeof(struct index_map));
    map = (struct index_map*) HT_ENTRY_DATA(entry);
    map->ptr = ptr;
    map->id = id;

    ht_add(table, entryp, hash, entry);
}

/*
 * Convert a symbol_type_t to a string for output.
 */
static const char*
symbol_type_to_string(symbol_type_t type)
{
    switch (type) {
    case symbol_type_identifier:        return "symbol_type_identifier";
    case symbol_type_variable:          return "symbol_type_variable";
    case symbol_type_symbolic_constant: return "symbol_type_symbolic_constant";
    case symbol_type_integer_constant:  return "symbol_type_integer_constant";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Emit a properly constructed DECLARE_SYMBOL() macro.
 */
static void
declare_symbol(FILE* file, symbol_t symbol)
{
    if (SYMBOL_IS_NIL(symbol))
        fprintf(file, "DECLARE_NIL_SYMBOL");
    else
        fprintf(file, "DECLARE_SYMBOL(%d, %s)",
                GET_SYMBOL_VALUE(symbol),
                symbol_type_to_string(GET_SYMBOL_TYPE(symbol)));
}

/*
 * Emit a properly constructed SYMBOL_TO_WORD() macro.
 */
static void
symbol_to_word(FILE* file, symbol_t symbol)
{
    fprintf(file, "SYMBOL_TO_WORD(%d, %s)",
            GET_SYMBOL_VALUE(symbol),
            symbol_type_to_string(GET_SYMBOL_TYPE(symbol)));
}

/*
 * Convert a beta_node_type_t to a string value for output.
 */
static const char*
beta_node_type_to_string(beta_node_type_t type)
{
    switch (type) {
    case beta_node_type_memory:
        return "beta_node_type_memory";

    case beta_node_type_positive_join:
        return "beta_node_type_positive_join";

    case beta_node_type_memory_positive_join:
        return "beta_node_type_memory_positive_join";

    case beta_node_type_negative:
        return "beta_node_type_negative";

    case beta_node_type_root:
        return "beta_node_type_root";

    case beta_node_type_conjunctive_negative:
        return "beta_node_type_conjunctive_negative";

    case beta_node_type_conjunctive_negative_partner:
        return "beta_node_type_conjunctive_negative_partner";

    case beta_node_type_production:
        return "beta_node_type_production";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Convert a test_type_t to a string value for output.
 */
static const char*
test_type_to_string(test_type_t type)
{
    switch (type) {
    case test_type_blank:            return "test_type_blank";
    case test_type_equality:         return "test_type_equality";
    case test_type_not_equal:        return "test_type_not_equal";
    case test_type_less:             return "test_type_less";
    case test_type_greater:          return "test_type_greater";
    case test_type_less_or_equal:    return "test_type_less_or_equal";
    case test_type_greater_or_equal: return "test_type_greater_or_equal";
    case test_type_same_type:        return "test_type_same_type";
    case test_type_disjunctive:      return "test_type_disjunctive";
    case test_type_conjunctive:      return "test_type_conjunctive";
    case test_type_goal_id:          return "test_type_goal_id";
    case test_type_impasse_id:       return "test_type_impasse_id";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Convert a relational_type_t to a string value for output.
 */
static const char*
relational_type_to_string(relational_type_t type)
{
    switch (type) {
    case relational_type_constant: return "relational_type_constant";
    case relational_type_variable: return "relational_type_variable";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Convert a field_t to a string value for output.
 */
static const char*
field_to_string(field_t field)
{
    switch (field) {
    case field_id:     return "field_id";
    case field_attr:   return "field_attr";
    case field_value:  return "field_value";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Emit a properly constructed VARIABLE_BINDING_TO_WORD() macro.
 */
static void
variable_binding_to_word(FILE* file, variable_binding_t binding)
{
    fprintf(file, "VARIABLE_BINDING_TO_WORD(%s, %d)",
            field_to_string(binding.field),
            binding.depth);
}

/*
 * Convert a preferenct_type_t to a string value for output.
 */
static const char*
preference_type_to_string(preference_type_t type)
{
    switch (type) {
    case preference_type_acceptable:         return "preference_type_acceptable";
    case preference_type_reject:             return "preference_type_reject";
    case preference_type_reconsider:         return "preference_type_reconsider";
    case preference_type_unary_indifferent:  return "preference_type_unary_indifferent";
    case preference_type_best:               return "preference_type_best";
    case preference_type_worst:              return "preference_type_worst";
    case preference_type_prohibit:           return "preference_type_prohibit";
    case preference_type_require:            return "preference_type_require";
    case preference_type_binary_indifferent: return "preference_type_binary_indifferent";
    case preference_type_better:             return "preference_type_better";
    case preference_type_worse:              return "preference_type_worse";
    default:
    }

    UNREACHABLE();
    return 0;
}

/*
 * Convert a rhs_value_type_t to a string value for output.
 */
static const char*
rhs_value_type_to_string(rhs_value_type_t type)
{
    switch (type) {
    case rhs_value_type_symbol:           return "rhs_value_type_symbol";
    case rhs_value_type_variable_binding: return "rhs_value_type_variable_binding";
    case rhs_value_type_unbound_variable: return "rhs_value_type_unbound_variable";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Convert a support_type_t to a string value for output
 */
static const char*
support_type_to_string(support_type_t type)
{
    switch (type) {
    case support_type_isupport:     return "support_type_isupport";
    case support_type_osupport:     return "support_type_osupport";
    case support_type_architecture: return "support_type_architecture";
    case support_type_unknown:      return "support_type_unknown";
    }

    UNREACHABLE();
    return 0;
}

/*
 * Collect the alpha nodes into an index map.
 */
static void
collect_alpha_nodes(struct agent* agent, struct ht* alphas)
{
    int id = 0;
    int i;

    for (i = 0; i < 16; ++i) {
        struct alpha_node* node = agent->alpha_nodes[i];
        while (node) {
            put_index_map_id(alphas, node, id++);
            node = node->siblings;
        }
    }
}

/*
 * Emit the alpha nodes.
 */
static void
export_alpha_nodes(FILE* file, struct agent* agent, struct ht* alphas, struct ht* betas)
{
    int i;
    for (i = 0; i < 16; ++i) {
        struct alpha_node* node = agent->alpha_nodes[i];
        while (node) {
            fprintf(file, "  { /* %d (%p) */\n    ", get_index_map_id(alphas, node), node);
            declare_symbol(file, node->id);
            fprintf(file, ",\n    ");
            declare_symbol(file, node->attr);
            fprintf(file, ",\n    ");
            declare_symbol(file, node->value);
            fprintf(file, ",\n    ");

            if (node->siblings)
                fprintf(file, "&alphas[%d], ", get_index_map_id(alphas, node->siblings));
            else
                fprintf(file, "0, ");

            if (node->children)
                fprintf(file, "&betas[%d], ", get_index_map_id(betas, node->children));
            else
                fprintf(file, "0, ");

            fprintf(file, "0 },\n");
            node = node->siblings;
        }
    }
}

/*
 * Collect an index map for the beta tests in a beta node.
 */
static void
collect_beta_tests(struct beta_test* test, struct ht* tests, int* id)
{
    put_index_map_id(tests, test, (*id)++);

    if (test->type == test_type_disjunctive) {
        struct beta_test* disjuncts = test->data.disjuncts;
        while (disjuncts) {
            collect_beta_tests(disjuncts, tests, id);
            disjuncts = disjuncts->next;
        }
    }

    if (test->next)
        collect_beta_tests(test->next, tests, id);
}

/*
 * Collect an index map for the actions in a production.
 */
static void
collect_actions(struct ht* actions, struct action* action, int* action_id)
{
    put_index_map_id(actions, action, (*action_id)++);

    if (action->next)
        collect_actions(actions, action->next, action_id);
}

/*
 * Collect index maps for the beta nodes, beta tests, productions, and
 * actions in the beta network.
 */
static void
collect_beta_nodes(struct beta_node* node,
                   struct ht*        betas,
                   struct ht*        tests,
                   struct ht*        productions,
                   struct ht*        actions,
                   int*              beta_id,
                   int*              test_id,
                   int*              prod_id,
                   int*              action_id)
{
    put_index_map_id(betas, node, (*beta_id)++);

    switch (node->type) {
    case beta_node_type_positive_join:
    case beta_node_type_memory_positive_join:
    case beta_node_type_negative:
        collect_beta_tests(node->data.tests, tests, test_id);
        break;

    case beta_node_type_production:
        put_index_map_id(productions, node->data.production, (*prod_id)++);
        collect_actions(actions, node->data.production->actions, action_id);
        break;

    default:
        break;
    }

    if (node->siblings)
        collect_beta_nodes(node->siblings,
                           betas, tests, productions, actions,
                           beta_id, test_id, prod_id, action_id);

    if (node->children)
        collect_beta_nodes(node->children,
                           betas, tests, productions, actions,
                           beta_id, test_id, prod_id, action_id);
}

/*
 * Walk the beta network emitting static initializers for beta nodes.
 */
static void
export_beta_nodes(FILE*             file,
                  struct beta_node* node,
                  struct ht*        alphas,
                  struct ht*        betas,
                  struct ht*        tests,
                  struct ht*        productions)
{
    fprintf(file, "  { /* %d (%p) */\n    %s,\n",
            get_index_map_id(betas, node),
            node,
            beta_node_type_to_string(node->type));

    fprintf(file, "    ");
    if (node->parent)
        fprintf(file, "&betas[%d], ", get_index_map_id(betas, node->parent));
    else
        fprintf(file, "0, ");

    if (node->siblings)
        fprintf(file, "&betas[%d], ", get_index_map_id(betas, node->siblings));
    else
        fprintf(file, "0, ");

    if (node->children)
        fprintf(file, "&betas[%d], ", get_index_map_id(betas, node->children));
    else
        fprintf(file, "0, ");

    fprintf(file, "\n    ");
    if (node->alpha_node)
        fprintf(file, "&alphas[%d], ", get_index_map_id(alphas, node->alpha_node));
    else
        fprintf(file, "0, ");

    if (node->next_with_same_alpha_node)
        fprintf(file, "&betas[%d],", get_index_map_id(betas, node->next_with_same_alpha_node));
    else
        fprintf(file, "0,");

    fprintf(file, "\n    0, 0, ");

    switch (node->type) {
    case beta_node_type_positive_join:
    case beta_node_type_memory_positive_join:
    case beta_node_type_negative:
        /* write test reference */
        fprintf(file, "&tests[%d]", get_index_map_id(tests, node->data.tests));
        break;

    case beta_node_type_production:
        /* write production reference */
        fprintf(file, "(struct beta_test*) &productions[%d]",
                get_index_map_id(productions, node->data.production));
        break;

    default:
        /* write empty */
        fprintf(file, "0");
        break;
    }

    fprintf(file, " },\n");

    if (node->siblings)
        export_beta_nodes(file, node->siblings, alphas, betas, tests, productions);

    if (node->children)
        export_beta_nodes(file, node->children, alphas, betas, tests, productions);
}

/*
 * Emit a static initializer for a beta_test.
 */
static void
export_beta_test(FILE* file, struct beta_test* test, struct ht* tests)
{
    fprintf(file, "  { /* %d (%p) */\n", get_index_map_id(tests, test), test);

    fprintf(file, "    %s, %s, %s,\n",
            test_type_to_string(test->type),
            relational_type_to_string(test->relational_type),
            field_to_string(test->field));

    fprintf(file, "    ");

    if (test->type != test_type_disjunctive) {
        if (test->relational_type == relational_type_constant) {
            symbol_to_word(file, test->data.constant_referent);
        }
        else {
            variable_binding_to_word(file, test->data.variable_referent);
        }
    }
    else {
        fprintf(file, "(unsigned) &tests[%d]",
                get_index_map_id(tests, test->data.disjuncts));
    }

    fprintf(file, " },\n");

    if (test->type == test_type_disjunctive) {
        struct beta_test* disjuncts = test->data.disjuncts;
        while (disjuncts) {
            export_beta_test(file, disjuncts, tests);
            disjuncts = disjuncts->next;
        }
    }

    if (test->next)
        export_beta_test(file, test->next, tests);
}

/*
 * Walk the beta network emitting static initializers for beta_test
 * structures as appropriate beta_nodes are discovered.
 */
static void
export_beta_tests(FILE* file, struct beta_node* node, struct ht* tests)
{
    if (node->type == beta_node_type_positive_join ||
        node->type == beta_node_type_memory_positive_join ||
        node->type == beta_node_type_negative)
        export_beta_test(file, node->data.tests, tests);

    if (node->siblings)
        export_beta_tests(file, node->siblings, tests);

    if (node->children)
        export_beta_tests(file, node->children, tests);
}

/*
 * Emit a static initializer for a rhs_value structure.
 */
static void
rhs_value(FILE* file, struct rhs_value* value)
{
    fprintf(file, "{ %s, ", rhs_value_type_to_string(value->type));

    switch (value->type) {
    case rhs_value_type_symbol:
        symbol_to_word(file, value->val.symbol);
        break;

    case rhs_value_type_variable_binding:
        variable_binding_to_word(file, value->val.variable_binding);
        break;

    case rhs_value_type_unbound_variable:
        fprintf(file, "%d", value->val.unbound_variable);
        break;

    default:
        UNREACHABLE();
    }

    fprintf(file, " }");
}

/*
 * Emit a static initializer for an action structure.
 */
static void
export_action(FILE* file, struct action* action, struct ht* actions)
{
    fprintf(file, "  { /* %d (%p) */\n    ", get_index_map_id(actions, action), action);

    if (action->next)
        fprintf(file, "&actions[%d], ", get_index_map_id(actions, action->next));
    else
        fprintf(file, "0, ");

    fprintf(file, "%s,\n    ", preference_type_to_string(action->preference_type));

    rhs_value(file, &action->id);
    fprintf(file, ",\n    ");
    rhs_value(file, &action->attr);
    fprintf(file, ",\n    ");
    rhs_value(file, &action->value);
    fprintf(file, ",\n    ");

    if (action->preference_type & preference_type_binary)
        rhs_value(file, &action->referent);
    else
        fprintf(file, "{ 0, 0 }");

    fprintf(file, " },\n");

    if (action->next)
        export_action(file, action->next, actions);
}

/*
 * Walk the beta network, emitting actions as production nodes are
 * discovered.
 */
static void
export_actions(FILE* file, struct beta_node* node, struct ht* actions)
{
    if (node->type == beta_node_type_production)
        export_action(file, node->data.production->actions, actions);

    if (node->siblings)
        export_actions(file, node->siblings, actions);

    if (node->children)
        export_actions(file, node->children, actions);
}

/*
 * Emit a static initializer for a production.
 */
static void
export_production(FILE*              file,
                  struct production* production,
                  struct ht*         productions,
                  struct ht*         actions)
{
    fprintf(file, "  { /* %d (%p) */\n", get_index_map_id(productions, production), production);
    fprintf(file, "#ifdef DEBUG\n");
    fprintf(file, "    \"%s\",\n", production->name);
    fprintf(file, "#endif\n");

    fprintf(file, "    0, ");
    if (production->actions)
        fprintf(file, "&actions[%d], ", get_index_map_id(actions, production->actions));
    else
        fprintf(file, "0, ");
    
    fprintf(file, "0, %d, %s },\n",
            production->num_unbound_vars,
            support_type_to_string(production->support));
}

/*
 * Walk the beta network, exporting each production node in order.
 */
static void
export_productions(FILE*             file,
                   struct beta_node* node,
                   struct ht*        productions,
                   struct ht*        actions)
{
    if (node->type == beta_node_type_production)
        export_production(file, node->data.production, productions, actions);

    if (node->siblings)
        export_productions(file, node->siblings, productions, actions);

    if (node->children)
        export_productions(file, node->children, productions, actions);
}

void
export_symtab(FILE* file, struct symtab* symtab)
{
    int i;
    for (i = 1; i < symtab->next_sym_constant; ++i) {
        symbol_t symbol;
        const char* name;

        MAKE_SYMBOL(symbol, symbol_type_symbolic_constant, i);
        name = symtab_find_name(symtab, symbol);

        fprintf(file, "#define SYM_");

        while (*name) {
            if (*name == '-')
                fputc('_', file);
            else if (*name == '*')
                fputs("_STAR_", file);
            else if (*name >= 'a' && *name <= 'z')
                fputc(*name - 'a' + 'A', file);
            else
                fputc(*name, file);

            ++name;
        }

        fprintf(file, " %d\n", i);
    }
}

/*
 * Write a static initializer for the agent
 */
void
export_agent(FILE* file, struct agent* agent, struct ht* alphas)
{
    int i;

    fprintf(file, "static struct agent agent = {\n");
    fprintf(file, "  1, &betas[0], { &betas[0], 0, 0, 0 },\n");
    fprintf(file, "  { ");

    for (i = 0; i < 16; ++i) {
        if (i > 0)
            fprintf(file, "    ");

        if (agent->alpha_nodes[i])
            fprintf(file, "&alphas[%d]", get_index_map_id(alphas, agent->alpha_nodes[i]));
        else
            fprintf(file, "0");

        if (i < 15)
            fprintf(file, ",\n");
    }

    fprintf(file, " },\n");

    fprintf(file, "  0, 0, 0, 0, { 0, 0, 0, 0 }, 0 };\n\n");
}

void
soar_export(FILE* file, struct agent* agent, struct symtab* symtab)
{
    struct ht alphas;
    struct ht betas;
    struct ht tests;
    struct ht productions;
    struct ht actions;
    int beta_id = 0;
    int test_id = 0;
    int prod_id = 0;
    int action_id = 0;
    
    ht_init(&alphas, (ht_key_compare_t) compare_map_entries);
    collect_alpha_nodes(agent, &alphas);

    ht_init(&betas, (ht_key_compare_t) compare_map_entries);
    ht_init(&tests, (ht_key_compare_t) compare_map_entries);
    ht_init(&productions, (ht_key_compare_t) compare_map_entries);
    ht_init(&actions, (ht_key_compare_t) compare_map_entries);

    collect_beta_nodes(agent->root_node,
                       &betas, &tests, &productions, &actions,
                       &beta_id, &test_id, &prod_id, &action_id);

    fprintf(file, "/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */\n\n");
    fprintf(file, "/* This file was automatically generated */\n\n");
    fprintf(file, "/* Symbolic constants */\n");
    export_symtab(file, symtab);
    fprintf(file, "\n\n");

    fprintf(file, "#ifdef DECL_RETE_NETWORK\n\n");
    fprintf(file, "static struct beta_node betas[];\n\n");

    fprintf(file, "static struct alpha_node alphas[] = {\n");
    export_alpha_nodes(file, agent, &alphas, &betas);
    fprintf(file, "};\n\n");

    fprintf(file, "static struct beta_test tests[] = {\n");
    export_beta_tests(file, agent->root_node, &tests);
    fprintf(file, "};\n\n");

    fprintf(file, "static struct action actions[] = {\n");
    export_actions(file, agent->root_node, &actions);
    fprintf(file, "};\n\n");

    fprintf(file, "static struct production productions[] = {\n");
    export_productions(file, agent->root_node, &productions, &actions);
    fprintf(file, "};\n\n");

    fprintf(file, "static struct beta_node betas[] = {\n");
    export_beta_nodes(file, agent->root_node, &alphas, &betas, &tests, &productions);
    fprintf(file, "};\n\n");

    export_agent(file, agent, &alphas);

    fprintf(file, "#endif /* DECL_RETE_NETWORK */\n");

    ht_finish(&actions, free_index_map, 0);
    ht_finish(&productions, free_index_map, 0);
    ht_finish(&tests, free_index_map, 0);
    ht_finish(&betas, free_index_map, 0);
    ht_finish(&alphas, free_index_map, 0);
}
