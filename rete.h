#ifndef rete_h__
#define rete_h__

struct variable_binding_list {
    symbol_t                      variable;
    variable_binding_t            binding;
    struct variable_binding_list* next;
};

/*
 * Given an id, attr, value, and alpha-type, determine which alpha
 * memory bucket a test should be in.
 */
static inline short
get_alpha_test_index(symbol_t id,
                     symbol_t attr,
                     symbol_t value,
                     wme_type_t type)
{
    return ((type == wme_type_acceptable) ? 8 : 0) |
        (GET_SYMBOL_VALUE(id) ? 4 : 0) |
        (GET_SYMBOL_VALUE(attr) ? 2 : 0) |
        (GET_SYMBOL_VALUE(value) ? 1 : 0);
}

/*
 * Determine if a working memory element matches an alpha node
 */
static inline bool_t
wme_matches_alpha_node(const struct wme* wme, const struct alpha_node* node)
{
    return (SYMBOL_IS_NIL(node->id) ||
            SYMBOLS_ARE_EQUAL(node->id, wme->slot->id)) &&
        (SYMBOL_IS_NIL(node->attr) ||
         SYMBOLS_ARE_EQUAL(node->attr, wme->slot->attr)) &&
        (SYMBOL_IS_NIL(node->value) ||
         SYMBOLS_ARE_EQUAL(node->value, wme->value));
}

extern void
add_wme_to_alpha_node(struct agent* agent,
                      struct alpha_node* node,
                      struct wme* wme);

void
do_left_addition(struct agent* agent,
                 struct beta_node* node,
                 struct token* token,
                 struct wme* wme);

void
do_left_removal(struct agent* agent,
                struct beta_node* node,
                struct token* token,
                struct wme* wme);

void
do_right_addition(struct agent* agent,
                  struct beta_node* node,
                  struct wme* wme);

void
do_right_removal(struct agent* agent,
                 struct beta_node* node,
                 struct wme* wme);

#endif /* rete_h__ */

