#include "globalInclude.h"

uint64_t* yuyan_print(uint64_t s[]) {
    fprintf(stdout,"%s", addr_to_string(s));
    fflush(stdout);
    return unit_to_addr();
}