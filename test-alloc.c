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

#include <stdio.h>
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

void
panic()
{
    printf("out of memory!\n");
}

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
        char *p, *q;
        p = malloc(1);
        q = malloc(3);
        free(q);
        free(p);
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
