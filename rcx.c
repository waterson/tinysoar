#include "rcx.h"

void
rcx_lcd_show_number(unsigned format, int value, unsigned scalecode)
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

