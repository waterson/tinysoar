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
 * Registers, flags, and subroutines for the Lego Mindstorms RCX. Much
 * of this code was culled and adapted from LegOS,
 *
 *   <http://legos.sourceforge.net/>
 *
 * and:
 *
 *   <http://www.daimi.au.dk/dArkOS/Vaerktoejer.dir/RCX.vejledning.dir/Vejledning.html>
 *     ``RCX Manual'', Ole Caprani <ocaprani@daimi.au.dk>
 *
 */

#ifndef rcx_h__
#define rcx_h__

/*
 * Timer control/status register
 */
extern volatile unsigned char T_CSR;

/*
 * TCSR bitmasks
 */
#define TCSR_ICA        0x80       /* input capture events */
#define TCSR_ICB        0x40
#define TCSR_ICC        0x20
#define TCSR_ICD        0x10
#define TCSR_OCA        0x08       /* output compare events */
#define TCSR_OCB        0x04
#define TCSR_OF         0x02       /* overflow event */
#define TCSR_RESET_ON_A 0x01       /* reset counter on match A */

/*
 * Timer control register
 */
extern unsigned char T_CR;

/*
 * TCR bitmasks
 */
#define TCR_A_RISING        0x80       /* input capture on rising */
#define TCR_B_RISING        0x40       /* edge. if not set -> lower */
#define TCR_C_RISING        0x20
#define TCR_D_RISING        0x10
#define TCR_BUFFER_A        0x08       /* buffer A in C */
#define TCR_BUFFER_B        0x04       /* buffer B in D */
#define TCR_CLOCK_2         0x00       /* clock = pclock / 2 */
#define TCR_CLOCK_8         0x01       /* clock = pclock / 8 */
#define TCR_CLOCK_32        0x02       /* clock = pclock / 32 */
#define TCR_CLOCK_EXT       0x03       /* external clock, rising edge */

/*
 * Timer output control register
 */
extern unsigned char T_OCR;

/*
 * TOCR bitmasks
 */
#define TOCR_OCRA               0x00       /* select register to write */
#define TOCR_OCRB               0x10
#define TOCR_ENABLE_A           0x08       /* enable output signals */
#define TOCR_ENABLE_B           0x04
#define TOCR_HIGH_LEVEL_A       0x02       /* set output to high for match */
#define TOCR_HIGH_LEVEL_B       0x01

/*
 * Timer output compare register A
 */
extern unsigned T_OCRA;

/*
 * Timer interrupt enable register
 */
extern unsigned char T_IER;

/*
 * TIER bitmasks
 */
#define TIER_ENABLE_ICA        0x80       /* input capture IRQ enables */
#define TIER_ENABLE_ICB        0x40
#define TIER_ENABLE_ICC        0x20
#define TIER_ENABLE_ICD        0x10
#define TIER_ENABLE_OCA        0x08       /* output compare IRQ enables */
#define TIER_ENABLE_OCB        0x04
#define TIER_ENABLE_OF         0x02       /* overflow IRQ enable */
#define TIER_RESERVED          0x01       /* always set. */

/*
 * OCIA interrupt vector.
 */
extern void *ocia_vector;

/*
 * Icons for use with rcx_(show|hide)_icon.
 */
#define LCD_STANDING         0x3006     /* standing figure         */
#define LCD_WALKING          0x3007     /* walking figure          */
#define LCD_SENSOR_0_VIEW    0x3008     /* sensor 0 view selected  */
#define LCD_SENSOR_0_ACTIVE  0x3009     /* sensor 0 active         */
#define LCD_SENSOR_1_VIEW    0x300a     /* sensor 1 view selected  */
#define LCD_SENSOR_1_ACTIVE  0x300b     /* sensor 1 active         */
#define LCD_SENSOR_2_VIEW    0x300c     /* sensor 2 view selected  */
#define LCD_SENSOR_2_ACTIVE  0x300d     /* sensor 2 active         */
#define LCD_MOTOR_0_VIEW     0x300e     /* motor 0 view selected   */
#define LCD_MOTOR_0_REV      0x300f     /* motor 0 backward arrow  */
#define LCD_MOTOR_0_FWD      0x3010     /* motor 0 forward arrow   */
#define LCD_MOTOR_1_VIEW     0x3011     /* motor 1 view selected   */
#define LCD_MOTOR_1_REV      0x3012     /* motor 1 backward arrow  */
#define LCD_MOTOR_1_FWD      0x3013     /* motor 1 forward arrow   */
#define LCD_MOTOR_2_VIEW     0x3014     /* motor 2 view selected   */
#define LCD_MOTOR_2_REV      0x3015     /* motor 2 backward arrow  */
#define LCD_MOTOR_2_FWD      0x3016     /* motor 2 forward arrow   */
#define LCD_DATALOG          0x3018     /* datalog indicator       */
#define LCD_DOWNLOAD         0x3019     /* download in progress    */
#define LCD_UPLOAD           0x301a     /* upload in progress      */ 
#define LCD_BATTERY          0x301b     /* battery low             */
#define LCD_RANGE_SHORT      0x301c     /* short range indicator   */
#define LCD_RANGE_LONG       0x301d     /* long range indicator    */
#define LCD_ALL              0x3020     /* all segments            */

/*
 * Show an icon on the RCX's LCD.
 */
static inline void
rcx_lcd_show_icon(unsigned icon)
{
    asm("\tmov.w %0,r6\n"
        "\tjsr lcd_show\n"
        "\tjsr lcd_refresh\n"
        :
        : "r"(icon)
        : "r6");
}

/*
 * Hide an icon on the RCX's LCD.
 */
static inline void
rcx_lcd_hide_icon(unsigned icon)
{
    asm("\tmov.w %0,r6\n"
        "\tjsr lcd_hide\n"
        "\tjsr lcd_refresh\n"
        :
        : "r"(icon)
        : "r6");
}

/*
 * Show a number on the RCX's LCD.
 */
extern void
rcx_lcd_show_number(unsigned format, int value, unsigned scalecode);

/*
 * Show a 16-bit signed integer value on the RCX's LCD.
 */
static inline void
rcx_lcd_show_int16(int r)
{ 
    rcx_lcd_show_number(0x3001, r, 0x3002);
}

/*
 * Clear the RCX's LCD.
 */
static inline void
rcx_lcd_clear()
{
    asm("\tjsr lcd_clear\n"
        "\tjsr lcd_refresh\n");
}


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

/*
 * Sensor I/O port.
 */
extern volatile unsigned char PORT6;
extern volatile unsigned char PORT6_DDR;
extern volatile unsigned char rom_port6_ddr;

#endif /* rcx_h__ */
