#include "alloc.h"

#define MIN_REQUEST SIZEOF_INT

static char *heap_start;
static char *heap_end;

struct block_header {
    unsigned block_size : BITS_PER_WORD - 1;
    unsigned prev_free : 1;
};

struct block_footer {
    struct block_header *header;
};

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

#if defined(RCX)
    debug_wait_button(6969);
#endif

    /* Uh oh, couldn't allocate! */
    return 0;
}

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
