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
 * An embedding harness for TinySoar that runs on the Lego Mindstorms
 * RCX. Much of the code here was cribbed from LegOS
 *
 *   <http://legos.sourceforge.net/>
 *
 * with minor adaptations.
 */
#include "alloc.h"
#include "soar.h"
#include "rcx.h"

#define DECL_RETE_NETWORK
#include "agent.inc"

/*
 * The RCX wants to find this string to unlock the firmware.
 */
unsigned char *firmware_string = "Do you byte, when I knock?";

#define RUNSTATE_POWER_ON 0x1
#define RUNSTATE_RUNNING  0x2
volatile unsigned char runstate = 0;

/*static*/ unsigned char timer = 10;

static volatile unsigned char keystate = 0;

#define KEY_ON_OFF  (1 << 0)
#define KEY_RUN     (1 << 1)
#define KEY_VIEW    (1 << 2)
#define KEY_PROGRAM (1 << 3)
#define KEY_ANY     (KEY_ON_OFF | KEY_RUN | KEY_VIEW | KEY_PROGRAM)

/*
 * Handler for the OCIA interrupt. Does key debouncing. Borrows
 * heavily from LegOS's dkey.c, which is Copyright (C) 1999 Markus
 * L. Noga <markus@noga.de>.
 */
static void
ocia_handler()
{
    asm(/* Check the debouncing timer. Has it decreased to zero? */
        "\tmov.b @_timer,r6l\n"
        "\tbeq check\n"

        /* Not yet, decrement it and bail. */
        "\tdec r6l\n"
        "\tmov.b r6l,@_timer\n"
        "\tjmp done\n"

        /* Load key status from PORT4 into bits zero and one of R6L. */
        "check:\n"
        "\tsub.b r6l,r6l\n"
        "\tmov.b @_PORT4,r6h\n"
        "\tbld #1,r6h\n"  /* ON-OFF */
        "\tbist #0,r6l\n"
        "\tbld #2,r6h\n"  /* RUN */
        "\tbist #1,r6l\n"

        /* Load key status from PORT7 into bits two and three of R6L. */
        "\tmov.b @_PORT7,r6h\n"
        "\tbld #6,r6h\n"
        "\tbist #2,r6l\n" /* VIEW */
        "\tbld #7,r6h\n"
        "\tbist #3,r6l\n" /* PROGRAM */

        /* Retrieve the keystate and see if anything has changed. */
        "\tmov.b @_keystate,r6h\n"
        "\txor.b r6l,r6h\n"
        "\tbeq done\n"

        /* Yup, store the new keystate. */
        "\tmov.b r6l,@_keystate\n"

#if 0 /* XXX if we ever want to remember the last key pressed... */
        /* Remeber the last single key pressed. */
        "\tand.b r6h,r6l\n"
        "\tmov.b r6l,@_key\n"
#endif

        /* Was the run key pressed? */
        "\tbtst #1,r6l\n"
        "\tbeq power\n"

        /* Yes: invert the RUNSTATE_RUNNING bit in the runstate. */
        "\tmov.b @_runstate,r6h\n"
        "\tbnot #1,r6h\n"
        "\tmov.b r6h,@_runstate\n"

        /* Was the power key pressed? */
        "power:\n"
        "\tbtst #0,r6l\n"
        "\tbeq reset\n"

        /* Yes: clear the RUNSTATE_POWER_ON bit in the runstate. */
        "\tmov.b @_runstate,r6h\n"
        "\tbclr #0,r6h\n"
        "\tmov.b r6h,@_runstate\n"

        /* Reset the debouncing timer. */
        "reset:\n"
        "\tmov.b #10,r6h\n"
        "\tmov.b r6h,@_timer\n"

        /* Clear the OCFA register so we keep getting interrupts. */
        "done:\n"
        "\tbclr #3,@_T_CSR:8\n"
        "\trts\n");
}

void
debug_wait_button(int value)
{
    rcx_lcd_show_int16(value);

    while (! (keystate & KEY_VIEW))
        asm("sleep");


    while (keystate & KEY_VIEW)
        asm("sleep");
}

void
panic()
{
    debug_wait_button(3507);
}

/*
 * Runtime heap setup.
 */
extern char mm_start;

char *addrs[] = {
    (char *) &mm_start, /* 0x8000-0xef30 (usable) */
    (char *) 0xef30,    /* 0xef30-0xef50 lcd data */
    (char *) 0xef50,    /* 0xef50-0xf000 (usable) */
    (char *) 0xf000,    /* 0xf000-0xf010 motor */
    (char *) 0xf010,    /* 0xf010-0xfb80 (usable) */
    (char *) 0xfb80,    /* 0xfb80-0xfe00 bad memory and vectors */
    (char *) 0xfe00,    /* 0xfe00-0xff00 (usable) */
    (char *) 0xff00     /* 0xff00-       stack */
};


/*
 * A structure for the sensor data.
 */
struct sensor {
    unsigned           symbol_value;
    unsigned           ad_csr_bits;
    volatile unsigned *ad_register;
    struct preference *preference;
};

struct sensor sensors[] = {
#ifdef SYM_SENSOR_1
    { SYM_SENSOR_1, 2, &AD_C, 0 },
#endif
#ifdef SYM_SENSOR_2
    { SYM_SENSOR_2, 1, &AD_B, 0 },
#endif
#ifdef SYM_SENSOR_3
    { SYM_SENSOR_3, 0, &AD_A, 0 },
#endif
};

#define NSENSORS (sizeof(sensors) / sizeof(sensors[0]))

/*
 * Synchronize the preference value for (s1 ^io.input-link.sensor-<n>).
 */
static void
sync_sensor(struct sensor *sensor)
{
    struct preference *pref = sensor->preference;
    unsigned sensor_val;
    symbol_t sym_input_link;
    symbol_t sym_sensor_n;
    symbol_t sym_value;

    /* Power down the sensor so we can read the channel. */
    PORT6 &= ~(1 << sensor->ad_csr_bits);

    /* Request an A/D conversion on the sensor's channel. */
    AD_CSR = sensor->ad_csr_bits;
    AD_CSR |= ADST;

    /* Wait for the conversion to complete. */
    while (! (AD_CSR & ADF))
        ;

    AD_CSR &= ~ADF;

    /* Read the sensor's value. */
    sensor_val = (*(sensor->ad_register) >> 6) & 0x03ff;

    /* Power the sensor back up. */
    PORT6 |= (1 << sensor->ad_csr_bits);

    /* If the value hasn't changed since we last set the preference,
       bail now. */
    if (pref && GET_SYMBOL_VALUE(pref->value) == sensor_val)
        return;

    /* Remove the preference for the old value, if there is one. */
    if (pref)
        wmem_remove_preference(&agent, pref);

    /* Create a preference for the new value. */
    INIT_SYMBOL(sym_input_link, symbol_type_identifier, 3 /*XXX*/);
    INIT_SYMBOL(sym_sensor_n, symbol_type_symbolic_constant, sensor->symbol_value);
    INIT_SYMBOL(sym_value, symbol_type_integer_constant, sensor_val);

    sensor->preference =
        wmem_add_preference(&agent,
                            sym_input_link, sym_sensor_n, sym_value,
                            preference_type_acceptable,
                            support_type_architecture);
}

/*
 * Synchronize the input-link with the sensors.
 */
static void
sync_input_link()
{
    struct sensor *sensor = sensors;
    struct sensor *limit = sensors + NSENSORS;
    for ( ; sensor < limit; ++sensor)
        sync_sensor(sensor);
}

/*
 * The motor interface.
 */
extern volatile unsigned char motor_controller;

/*
 * A structure for the motor data.
 */
struct motor {
    unsigned      symbol_value;
    unsigned char pattern[4];
};

struct motor motors[] = {
#ifdef SYM_MOTOR_A
    { SYM_MOTOR_A, { 0x00, 0x80, 0x40, 0xc0 } },
#endif
#ifdef SYM_MOTOR_B
    { SYM_MOTOR_B, { 0x00, 0x08, 0x04, 0x0c } },
#endif
#ifdef SYM_MOTOR_C
    { SYM_MOTOR_C, { 0x00, 0x02, 0x01, 0x03 } },
#endif
};

#define NMOTORS (sizeof(motors) / sizeof(motors[0]))

typedef enum motor_direction {
  motor_direction_off = 0,
  motor_direction_forward = 1,
  motor_direction_reverse = 2,
  motor_direction_brake = 3
} motor_direction_t;

/*
 * Synchronize the motor state with the value from
 * (s1 ^io.output-link.motor-<n>)
 */
static void
sync_motor(struct motor *motor)
{
    motor_direction_t direction = motor_direction_off;
    unsigned char output;
    struct wme *wme;
    symbol_t sym_output_link;
    symbol_t sym_motor_n;

    /* See if there's a wme for the motor. */
    INIT_SYMBOL(sym_output_link, symbol_type_identifier, 4 /*XXX*/);
    INIT_SYMBOL(sym_motor_n, symbol_type_symbolic_constant, motor->symbol_value);

    wme = wmem_get_wmes(&agent, sym_output_link, sym_motor_n);

    /* If we find one, and it's got an integer value, then set the
       direction appropriately. */
    if (wme) {
        switch (GET_SYMBOL_VALUE(wme->value)) {
#ifdef SYM_FORWARD
        case SYM_FORWARD:
            direction = motor_direction_forward;
            break;
#endif

#ifdef SYM_REVERSE
        case SYM_REVERSE:
            direction = motor_direction_reverse;
            break;
#endif

#ifdef SYM_BRAKE
        case SYM_BRAKE:
            direction = motor_direction_brake;
            break;
#endif

        default:
            /* Either SYM_OFF or something random. Leave the default
               value of motor_direction_off untouched. */
            break;
        }
    }

    /* Compute and output the waveform. */
    output = motor_controller;
    output &= ~(motor->pattern[motor_direction_brake]);
    output |= motor->pattern[direction];
    motor_controller = output;
}

/*
 * Synchronize the motors with the output-link.
 */
static void
sync_output_link()
{
    struct motor *motor = motors;
    struct motor *limit = motors + NMOTORS;
    for ( ; motor < limit; ++motor)
        sync_motor(motor);
}

extern char __bss, __bss_end;

static void
bss_init()
{
    char *p;
    for (p = &__bss; p < &__bss_end; ++p)
        *p = 0;
}

__attribute__((noreturn))
void
_start()
{
    /* Do one-time startup when the firmware is first installed. */
    bss_init();
    heap_init(addrs, sizeof addrs / sizeof addrs[0]);
    agent_init(&agent);

    while (1) {
        /* Power up and disable the software standby so we can safely
           issue the `sleep' instruction when idling. */
        asm("\tpush r6\n"
            "\tjsr power_init\n"
            "\tbclr #7,@_SYSCR:8\n"
            "\tpop r6\n");

        /* Make sure the motors are off. */
        motor_controller = 0;

        /* Configure the 16-bit timer compare A IRQ to occur every
           10ms. */
        T_CSR = TCSR_OCA | TCSR_RESET_ON_A;
        T_CR = TCR_CLOCK_32;
        T_OCR &= ~TOCR_OCRB;
        T_OCRA = 5000;

        ocia_vector = ocia_handler;
        T_IER |= TIER_ENABLE_OCA;

        /* Wait for the power key to be released. */
        keystate = KEY_ANY;
        while (keystate & KEY_ON_OFF)
            ;

        /* Run until the power gets turned off. */
        runstate |= RUNSTATE_POWER_ON;

        while (runstate & RUNSTATE_POWER_ON) {
            /* Standing man. */
            rcx_lcd_show_icon(LCD_STANDING);

            while ((runstate & RUNSTATE_POWER_ON) && !(runstate & RUNSTATE_RUNNING))
                asm("sleep"); /* Idle. */

            /* Running Man. */
            rcx_lcd_show_icon(LCD_WALKING);

            /* Power up active sensors. */
            PORT6_DDR = rom_port6_ddr |= 0x7;
            PORT6 |= 0x7;

            while ((runstate & RUNSTATE_POWER_ON) && (runstate & RUNSTATE_RUNNING)) {
                agent_elaborate(&agent);
                sync_output_link();
                sync_input_link();
            }

            /* Power down active sensors. */
            PORT6_DDR = rom_port6_ddr &= ~0x7;
            PORT6 &= ~0x7;

            /* Shut off the motors. */
            motor_controller = 0;
        }

        runstate &= ~RUNSTATE_RUNNING;

        agent_reset(&agent);

        /* Stop our timer interrupt. */
        T_IER &= ~TIER_ENABLE_OCA;

        /* Clear the display. */
        rcx_lcd_clear();

        /* Power down after re-enabling software stand-by mode. */
        asm("\tpush r6\n"
            "\tbset #7,@_SYSCR:8\n"
            "\tjsr power_off\n"
            "\tpop r6\n");
    }
}

