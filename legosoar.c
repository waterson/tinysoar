#include "soar.h"

#define DECL_RETE_NETWORK
#include "agent.inc"

/* Torn from legOS, Copyright (C) 1999 Markus L. Noga. */

/* A/D converter registers */
extern volatile unsigned AD_A;
extern volatile unsigned AD_B;
extern volatile unsigned AD_C;

#define SENSOR_1 AD_C
#define SENSOR_2 AD_B
#define SENSOR_3 AD_A

#define ds_scale(x)   ((unsigned int)(x)>>6)
#define ds_unscale(x) ((unsigned int)(x)<<6)

#define LIGHT(a) (147 - ds_scale(a)/7)

struct motor_state {
  union {
    unsigned assembler;
    struct {
      unsigned char delta;
      volatile unsigned char sum;
    } c;
  } access;
  unsigned char dir;
};

const unsigned char dm_a_pattern[] = {0xc0,0x40,0x80,0x00};
const unsigned char dm_b_pattern[] = {0x0c,0x04,0x08,0x00};
const unsigned char dm_c_pattern[] = {0x03,0x01,0x02,0x00};

struct motor_state dm_a, dm_b, dm_c;

typedef enum motor_direction {
  motor_direction_off = 0,
  motor_direction_forward = 1,
  motor_direction_reverse = 2,
  motor_directionbrake = 3
} motor_direction_t;

static void
sync_sensor_a(struct preference **sensor_pref)
{
    struct preference *pref = *sensor_pref;
    unsigned sensor_a_val = LIGHT(SENSOR_1);
    symbol_t sym_input_link;
    symbol_t sym_sensor_a;
    symbol_t sym_value;

    if (pref && pref->value.val == sensor_a_val)
        return;

    MAKE_SYMBOL(sym_input_link, 3 /*XXX*/, symbol_type_identifier);
    MAKE_SYMBOL(sym_sensor_a, SYM_SENSOR_A, symbol_type_symbolic_constant);
    MAKE_SYMBOL(sym_value, sensor_a_val, symbol_type_integer_constant);

    if (pref)
        wmem_remove_preference(&agent, pref);

    *sensor_pref =
        wmem_add_preference(&agent,
                            sym_input_link, sym_sensor_a, sym_value,
                            preference_type_acceptable,
                            support_type_architecture);
}

static void
sync_motor(unsigned motor, struct motor_state *state, const unsigned char pattern[])
{
    struct wme *wme;
    symbol_t sym_output_link;
    symbol_t sym_motor_n;

    MAKE_SYMBOL(sym_output_link, 4 /*XXX*/, symbol_type_identifier);
    MAKE_SYMBOL(sym_motor_n, motor, symbol_type_symbolic_constant);

    wme = wmem_get_wmes(&agent, sym_output_link, sym_motor_n);

    while (wme) {
        if (GET_SYMBOL_TYPE(wme->value) == symbol_type_integer_constant) {
            unsigned speed = GET_SYMBOL_VALUE(wme->value);
            if (speed > 0) {
                state->dir = pattern[motor_direction_forward];
            }
            else if (speed < 0) {
                speed = -speed;
                state->dir = pattern[motor_direction_reverse];
            }
            else {
                state->dir = pattern[motor_direction_off];
            }

            state->access.c.delta = speed;
            break;
        }

        wme = wme->next;
    }

    if (! wme) {
        state->dir = pattern[motor_direction_off];
        state->access.c.delta = 0;
    }
}

void kmain()
{
    struct preference *sensor_a_pref = 0;

    agent_init(&agent);

    while (1) {
        sync_sensor_a(&sensor_a_pref);
        agent_elaborate(&agent);
        sync_motor(SYM_MOTOR_A, &dm_a, dm_a_pattern);
        sync_motor(SYM_MOTOR_C, &dm_c, dm_c_pattern);
    }
}
