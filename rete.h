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
 * RETE runtime routines that are used from the RETE creation code.
 *
 * The RETE creation code (in prod.c) requires access to some of the
 * routines in the runtime code (in rete.c). This header exposes that
 * functionality.
 */

#ifndef rete_h__
#define rete_h__

/*
 * A list of variable bindings.
 */
struct variable_binding_list {
    symbol_t                      variable;
    variable_binding_t            binding;
    struct variable_binding_list *next;
};

/*
 * Given an id, attr, value, and alpha-type, determine which alpha
 * memory bucket a test should be in.
 */
static inline short
get_alpha_test_index(symbol_t   id,
                     symbol_t   attr,
                     symbol_t   value,
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
wme_matches_alpha_node(const struct wme *wme, const struct alpha_node *node)
{
    return (SYMBOL_IS_NIL(node->id) ||
            SYMBOLS_ARE_EQUAL(node->id, wme->slot->id)) &&
        (SYMBOL_IS_NIL(node->attr) ||
         SYMBOLS_ARE_EQUAL(node->attr, wme->slot->attr)) &&
        (SYMBOL_IS_NIL(node->value) ||
         SYMBOLS_ARE_EQUAL(node->value, wme->value));
}

/*
 * Add a working memory element to an alpha node.
 */
extern void
add_wme_to_alpha_node(struct agent      *agent,
                      struct alpha_node *node,
                      struct wme        *wme);

/*
 * Do a left-addition to a beta node.
 */
void
do_left_addition(struct agent     *agent,
                 struct beta_node *node,
                 struct token     *token,
                 struct wme       *wme);

/*
 * Do a right-memory addition to a beta node.
 */
void
do_right_addition(struct agent     *agent,
                  struct beta_node *node,
                  struct wme       *wme);

#endif /* rete_h__ */

