#include "parser.h"
#include "symtab.h"

int
main(int argc, char* argv[])
{
    struct symtab tab;
    struct production* p;

    symtab_init(&tab);

    p = soar_parse_rule(&tab,
                        "(state <s> ^superstate nil)\n\
                         -->\n\
                         (<s> ^operator <o> +)\n\
                         (<o> ^name wait)");

    return 0;
}
