#include "alloc.h"
#include "soar.h"

#define DECL_RETE_NETWORK
#include "agent.inc"

/*
 * Debugging stuff
 */
void
lcd_show_number(unsigned format, int value, unsigned scalecode)
{
    asm("\tpush r2\n"
        "\tpush r1\n"
        "\tmov.w r0,r6\n"
        "\tjsr lcd_number  ; call show_number\n"
        "\tadds #0x2,r7    ; cleanup stack \n"
        "\tadds #0x2,r7\n"
        "\tjsr lcd_refresh ; call refresh\n"
       );
}

extern volatile unsigned char PORT4;

void
debug_wait_button(int value)
{
    lcd_show_int16(value);

    /* Wait for the button to be pressed */
    while (! (PORT4 & (1 << 2)))
        ;

    /* Wait for the button to be released */
    while (PORT4 & (1 << 2))
        ;
}

/*
 * The RCX wants to find this string to unlock the firmware.
 */
unsigned char *firmware_string = "Do you byte, when I knock?";

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
 * A/D converter registers.
 */
extern volatile unsigned char AD_CSR;
extern volatile unsigned AD_A;
extern volatile unsigned AD_C;
extern volatile unsigned AD_B;

/*
 * Flags for the AD_CSR register.
 */
#define ADF   (1 << 7)
#define ADIE  (1 << 6)
#define ADST  (1 << 5)
#define SCAN  (1 << 4)
#define CKS   (1 << 3)
#define CH2   (1 << 2)
#define CH1   (1 << 1)
#define CH0   (1 << 0)

#define SENSOR_1 AD_C
#define SENSOR_2 AD_B
#define SENSOR_3 AD_A

/*
 * Motor interface
 */
extern volatile unsigned char motor_controller;

const unsigned char dm_a_pattern[] = { 0x00, 0x80, 0x40, 0xc0 };
const unsigned char dm_b_pattern[] = { 0x00, 0x08, 0x04, 0x0c };
const unsigned char dm_c_pattern[] = { 0x00, 0x02, 0x01, 0x03 };

typedef enum motor_direction {
  motor_direction_off = 0,
  motor_direction_forward = 1,
  motor_direction_reverse = 2,
  motor_direction_brake = 3
} motor_direction_t;

/*
 * Synchronize the preference value for (s1 ^io.input-link.sensor-1).
 */
static void
sync_sensor_1(struct preference **sensor_pref)
{
    struct preference *pref = *sensor_pref;
    unsigned sensor_1_val;
    symbol_t sym_input_link;
    symbol_t sym_sensor_1;
    symbol_t sym_value;

    AD_CSR = 2;
    AD_CSR |= ADST;
    while (! (AD_CSR & ADF))
        ;
    AD_CSR &= ~ADF;

    sensor_1_val = (AD_C >> 6) & 0x03ff;

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

/*
 * Synchronize the motor state with the value from
 * (s1 ^io.output-link.motor-x)
 */
static void
sync_motor(unsigned motor, const unsigned char pattern[])
{
    static unsigned char output = 0;
    struct wme *wme;
    symbol_t sym_output_link;
    symbol_t sym_motor_n;
    motor_direction_t direction = motor_direction_off;

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

    output &= ~(pattern[motor_direction_brake]);
    output |= pattern[direction];

    motor_controller = output;
}

static struct preference *sensor_1_pref = 0;

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
    motor_controller = 0;

    bss_init();
    heap_init(addrs, sizeof addrs / sizeof addrs[0]);
    agent_init(&agent);

    do {
        agent_elaborate(&agent);

        sync_motor(SYM_MOTOR_A, dm_a_pattern);
        sync_motor(SYM_MOTOR_C, dm_c_pattern);

        sync_sensor_1(&sensor_1_pref);
    } while (1);
}

