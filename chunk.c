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

#include "soar.h"
#include "alloc.h"

struct preference_list {
    struct preference      *preference;
    struct preference_list *next;
};

static void
chunk(struct agent           *agent,
      struct instantiation   *inst,
      int                     level,
      struct preference_list *results)
{
}

/*
 * Compute the transitive closure of the specified result set.
 */
static void
close_results(struct agent            *agent,
              int                      level,
              struct preference_list **results)
{
}

/*
 * Append the specified preference to the result set if it refers to a
 * goal above the specified level.
 */
static void
append_if_result(struct agent            *agent,
                 struct preference_list **results,
                 int                      level,
                 struct preference       *pref)
{
    int id_level = agent_get_id_level(agent, pref->slot->id);
    if (id_level && id_level < level) {
        /* Include the preference as a result. */
        struct preference_list *entry =
            (struct preference_list *) malloc(sizeof(struct preference_list));

        entry->preference = pref;
        entry->next       = *results;
        *results          = entry;
    }
}

/*
 * Examine the instantiation to determine if any results were returned
 * to a higher goal level. If so, create a chunk.
 */
void
chunk_if_results(struct agent         *agent,
                 struct instantiation *inst,
                 struct preference    *o_rejects)
{
    struct preference_list *results;
    struct preference *pref;
    int level;
    struct token *token;

    /* Compute the instantiation's `level'; i.e., the lowest goal
       level at which a token matched. */
    level = 0;
    for (token = inst->token; token != 0; token = token->parent) {
        if (token->wme) {
            int id_level = agent_get_id_level(agent, token->wme->slot->id);

            ASSERT(id_level != 0, ("identifier without assigned level"));

            if (id_level > level)
                level = id_level;
        }
    }

    /* Based on the goal level of the instantiation, determine if the
       instantation created any results for higher goals. */
    results = 0;
    for (pref = inst->preferences.next_in_instantiation;
         pref != &inst->preferences;
         pref = pref->next_in_instantiation) {
        append_if_result(agent, &results, level, pref);
    }

    for (pref = o_rejects; pref != 0; pref = pref->next_in_slot)
        append_if_result(agent, &results, level, pref);

    /* If we created results, then compute the transitive closure of
       the result set and build a chunk. */
    if (results) {
        struct preference_list *doomed;

        close_results(agent, level, &results);
        chunk(agent, inst, level, results);

        do {
            doomed = results;
            results = results->next;
            free(doomed);
        } while (results);
    }
}

