#include "config.h"
#undef HAVE_MALLOC_H

#include "alloc.h"

char heap[0x10000];

char *addrs[] = {
    heap + 0x8000, /* 0x8000-0xef30 (usable) */
    heap + 0xef30, /* 0xef30-0xef50 lcd data */
    heap + 0xef50, /* 0xef50-0xf000 (usable) */
    heap + 0xf000, /* 0xf000-0xf010 motor */
    heap + 0xf010, /* 0xf010-0xfb80 (usable) */
    heap + 0xfb80, /* 0xfb80-0xfe00 bad memory and vectors */
    heap + 0xfe00, /* 0xfe00-0xff00 (usable) */
    heap + 0xff00  /* 0xff00-       stack */
};

int
main(int argc, char *argv[])
{
    heap_init(addrs, sizeof addrs / sizeof addrs[0]);

    {
        char *p;
        p = malloc(16);
        free(p);
    }

    {
        char *p, *q;
        p = malloc(16);
        q = malloc(16);
        free(p);
        free(q);
    }

    {
        int i;
        char *p[16];

        for (i = 0; i < 16; ++i)
            p[i] = malloc(16);

        for (i = 0; i < 16; i += 2)
            free(p[i]);

        for (i = 1; i < 16; i += 2)
            free(p[i]);
    }

    return 0;
}
