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

#include "config.h"
#undef HAVE_MALLOC_H

#include "soar.h"
#include "alloc.h"

#define DECL_RETE_NETWORK
#include "agent.inc"

#include <stdio.h>

char heap[0x10000];

char *addrs[] = {
    (char *) heap + 0x8000, /* 0x8000-0xef30 (usable) */
    (char *) heap + 0xef30,    /* 0xef30-0xef50 lcd data */
    (char *) heap + 0xef50,    /* 0xef50-0xf000 (usable) */
    (char *) heap + 0xf000,    /* 0xf000-0xf010 motor */
    (char *) heap + 0xf010,    /* 0xf010-0xfb80 (usable) */
    (char *) heap + 0xfb80,    /* 0xfb80-0xfe00 bad memory and vectors */
    (char *) heap + 0xfe00,    /* 0xfe00-0xff00 (usable) */
    (char *) heap + 0xff00     /* 0xff00-       stack */
};

const unsigned char dm_a_pattern[] = { 0xc0, 0x40, 0x80, 0x00 };
const unsigned char dm_b_pattern[] = { 0x0c, 0x04, 0x08, 0x00 };
const unsigned char dm_c_pattern[] = { 0x03, 0x01, 0x02, 0x00 };

typedef enum motor_direction {
  motor_direction_off = 0,
  motor_direction_forward = 1,
  motor_direction_reverse = 2,
  motor_direction_brake = 3
} motor_direction_t;

static void
sync_sensor_1(struct preference **sensor_pref)
{
    struct preference *pref = *sensor_pref;
    static unsigned sensor_1_val = 1023;
    symbol_t sym_input_link;
    symbol_t sym_sensor_1;
    symbol_t sym_value;

    if (pref && GET_SYMBOL_VALUE(pref->value) == sensor_1_val)
        return;

    INIT_SYMBOL(sym_input_link, symbol_type_identifier, 3 /*XXX*/);
    INIT_SYMBOL(sym_sensor_1, symbol_type_symbolic_constant, SYM_SENSOR_1);
    INIT_SYMBOL(sym_value, symbol_type_integer_constant, sensor_1_val);

    if (pref)
        wmem_remove_preference(&agent, pref);

    *sensor_pref =
        wmem_add_preference(&agent,
                            sym_input_link, sym_sensor_1, sym_value,
                            preference_type_acceptable,
                            support_type_architecture);
}

static void
sync_motor(unsigned motor, const unsigned char pattern[])
{
    struct wme *wme;
    symbol_t sym_output_link;
    symbol_t sym_motor_n;
    motor_direction_t direction = motor_direction_off;

    INIT_SYMBOL(sym_output_link, symbol_type_identifier, 4 /*XXX*/);
    INIT_SYMBOL(sym_motor_n, symbol_type_symbolic_constant, motor);

    wme = wmem_get_wmes(&agent, sym_output_link, sym_motor_n);

    while (wme) {
        if (GET_SYMBOL_TYPE(wme->value) == symbol_type_integer_constant) {
            int speed = GET_SYMBOL_VALUE(wme->value);
            if (speed > 0)
                direction = pattern[motor_direction_forward];
            else if (speed < 0)
                direction = pattern[motor_direction_reverse];
            else
                direction = pattern[motor_direction_off];

            break;
        }

        wme = wme->next;
    }

    printf("direction = %08x\n", direction);
}

int
main(int argc, char *argv[])
{
    struct preference *sensor_1_pref = 0;

    heap_init(addrs, sizeof addrs / sizeof addrs[0]);
    agent_init(&agent);

    do {
        agent_elaborate(&agent);

        sync_motor(SYM_MOTOR_A, dm_a_pattern);
        sync_motor(SYM_MOTOR_C, dm_c_pattern);

        sync_sensor_1(&sensor_1_pref);
    } while (1);
}

const char *
symtab_find_name(struct symtab *symtab, symbol_t symbol)
{
    return "(unknown)";
}
