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

    if (pref && pref->value.val == sensor_1_val)
        return;

    MAKE_SYMBOL(sym_input_link, symbol_type_identifier, 3 /*XXX*/);
    MAKE_SYMBOL(sym_sensor_1, symbol_type_symbolic_constant, SYM_SENSOR_1);
    MAKE_SYMBOL(sym_value, symbol_type_integer_constant, sensor_1_val);

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
    motor_direction_t direction;

    MAKE_SYMBOL(sym_output_link, symbol_type_identifier, 4 /*XXX*/);
    MAKE_SYMBOL(sym_motor_n, symbol_type_symbolic_constant, motor);

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

    if (! wme)
        direction = motor_direction_off;

    printf("direction = %08x\n", direction);
}

int
main(int argc, char *argv[])
{
    struct preference *sensor_1_pref = 0;

    heap_init(addrs, sizeof addrs / sizeof addrs[0]);
    agent_init(&agent);

    while (1) {
        agent_elaborate(&agent);

        sync_motor(SYM_MOTOR_A, dm_a_pattern);
        sync_motor(SYM_MOTOR_C, dm_c_pattern);

        sync_sensor_1(&sensor_1_pref);
    }
}

const char *
symtab_find_name(struct symtab *symtab, symbol_t symbol)
{
    return "(unknown)";
}
