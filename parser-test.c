#include "parser.h"
#include "symtab.h"
#include <stdio.h>
#include <string.h>

static char buf[4096];

static void
parse_from_file(FILE* in)
{
    struct symtab tab;
    struct production* prod;
    int n = sizeof(buf);
    char* p = buf;

    symtab_init(&tab);

    while (fgets(p, n, in)) {
        int len = strlen(p);
        n -= len;
        p += len;

        if (n <= 0) {
            fprintf(stderr, "file too big");
            return;
        }
    }

    prod = soar_parse_rule(&tab, buf);

    /* XXX cleanup */

    symtab_finish(&tab);
}

int
main(int argc, char* argv[])
{
    if (argc > 1) {
        int i;
        for (i = 1; i < argc; ++i) {
            FILE* in = fopen(argv[i], "r");
            if (in) {
                parse_from_file(in);
                fclose(in);
            }
            else fprintf(stderr, "could not open %s\n", argv[i]);
        }
    }
    else parse_from_file(stdin);

    return 0;
}
