#include "parser.h"

int
main(int argc, char* argv[])
{
    struct production* p =
        soar_parse_rule("(state <s> ^superstate nil)\n\
                         -->\n\
                         (<s> ^operator <o> +)\n\
                         (<o> ^name wait)");

    return 0;
}
