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
 * A dead-simple first-fit allocator for a system without virtual
 * memory.
 *
 * Blocks have a one-word header that maintains the block size and
 * whether or not the previous block is free. Free blocks have a
 * footer that points to the block's header.
 *
 * Allocation is really dumb. We start at the first block and scan
 * forward until we find a free block that's large enough for our
 * request. (This could trivially be made better by threading the free
 * blocks into a freelist, but I'm lazy.)
 *
 * When a block is freed, we look at the previous and next blocks,
 * coalescing them with the current block if possible.
 */
#include "alloc.h"

static char *heap_start;
static char *heap_end;

struct block_header {
    unsigned block_size : BITS_PER_WORD - 1;
    unsigned prev_free : 1;
};

struct block_footer {
    struct block_header *header;
};

/*
 * The minimum request size.
 */
#define MIN_REQUEST sizeof(struct block_footer)

/*
 * Initialize the heap, given addresses that specify alternating
 * usable/unusable ranges of memory. The first address is the start of
 * the first usable region, the second address is the end of the first
 * usable region; the third address is the start of the next usable
 * region, the fourth address is its end, and so on.
 *
 * The addresses must be in ascending order.
 */
void
heap_init(char *addrs[], int naddrs)
{
    ASSERT(naddrs % 2 == 0, ("expected even number of addresses"));

    heap_start = *addrs;

    while (naddrs > 0) {
        struct block_header *header;
        struct block_footer *footer;
        char *begin, *end;
        unsigned size;

        begin = *(addrs++);
        end = *(addrs++);
        naddrs -= 2;

        /* This block's size is the size of the raw space less its
           header. We also subtractthe header for the next `block',
           which is reserved. */
        size = (end - begin) - (2 * sizeof(struct block_header));

        /* Fill in the header and footer information for the current
           block, which is free. */
        header = (struct block_header *) begin;
        header->block_size = size;
        header->prev_free = 0;

        footer = (struct block_footer *)
            (begin + sizeof(struct block_header) + size
             - sizeof(struct block_footer));

        footer->header = header;

        /* Fill in the header information for the next block, which is
           reserved. */
        header = (struct block_header *) (end - sizeof(struct block_header));
        header->block_size = (naddrs == 0) ? 0 : *addrs - end;
        header->prev_free = 1;
    }

    heap_end = *(--addrs);
}

/*
 * Scan the heap until we find a block large enough to fulfill the
 * request.
 */
void *
malloc(unsigned sz)
{
    struct block_header *header, *next;

    /* Align the request. */
    sz += MIN_REQUEST - 1;
    sz &= ~(MIN_REQUEST - 1);

    /* First fit. */
    for (header = (struct block_header *) heap_start;
         (char *) header < heap_end;
         header = next) {
        next = (struct block_header *)
            ((char *) header
             + sizeof(struct block_header)
             + header->block_size);

        if (next->prev_free && sz <= header->block_size) {
            /* We can fit the request in this block. */
            void *result = (void *)((char *) header + sizeof(struct block_header));

            if (header->block_size < sz + sizeof(struct block_header) + MIN_REQUEST) {
                /* We can't fit any other requests here, though. */
                next->prev_free = 0;
            }
            else {
                /* Split the block. */
                struct block_footer *footer =
                    (struct block_footer *)
                    ((char *) next - sizeof(struct block_footer));

                unsigned remaining = header->block_size - sz - sizeof(struct block_header);

                header->block_size = sz;

                header = (struct block_header *)
                    ((char *) header
                     + sizeof(struct block_header)
                     + sz);

                header->block_size = remaining;
                header->prev_free = 0;

                footer->header = header;
            }

            return result;
        }
    }

    /* Uh oh, couldn't allocate! */
    panic();
    return 0;
}

/*
 * Free the block, coalescing with the previous and next blocks if
 * possible.
 */
void
free(void *ptr)
{
    struct block_header *header = (struct block_header *)
        ((char *) ptr - sizeof(struct block_header));

    struct block_header *next = (struct block_header *)
        ((char *) ptr + header->block_size);

    struct block_header *next_next = (struct block_header *)
        ((char *) next + sizeof(struct block_header) + next->block_size);

    struct block_footer *footer;

    if ((char *) next_next < heap_end && next_next->prev_free) {
        /* The block following us is free. */
        next = next_next;
    }

    if (header->prev_free) {
        /* The block prior to us is free. */
        footer = (struct block_footer *)
            ((char *) header - sizeof(struct block_footer));

        header = footer->header;
    }

    footer = (struct block_footer *)
        ((char *) next - sizeof(struct block_footer));

    footer->header = header;

    header->block_size =
        (char *) next - (char *) header - sizeof(struct block_header);

    next->prev_free = 1;
}

#if defined(DEBUG) && defined(HAVE_PRINTF)
#include <stdio.h>

/*
 * Dump the heap to debug it.
 */
void
heap_walk()
{
    struct block_header *header, *next;

    printf("heap_begin=0x%p, heap_end=0x%p\n",
           heap_start, heap_end);

    for (header = (struct block_header *) heap_start;
         (char *) header < heap_end;
         header = next) {
        next = (struct block_header *)
            ((char *) header
             + sizeof(struct block_header)
             + header->block_size);

        printf("%p heap_start+%04x size=%04x %s",
               header,
               (char *) header - heap_start,
               header->block_size,
               next->prev_free ? "free" : "in use");

        if (next->prev_free) {
            struct block_footer *footer =
                (struct block_footer *)
                ((char *) next - sizeof(struct block_footer));

            if (footer->header != header) {
                printf(" BAD FOOTER, footer->header=%p heap_start+%04x",
                       footer->header,
                       (char *) footer->header - heap_start);
            }
        }

        printf("\n");
    }
}

#endif
