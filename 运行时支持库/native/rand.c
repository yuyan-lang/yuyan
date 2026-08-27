#include "common_include.h"

yyvalue yyGetRandomInt(yyvalue upperBoundPtr) {
    uint64_t upperBoundInt = yyvalue_to_int(upperBoundPtr);
    return int_to_yyvalue(rand() % upperBoundInt);
}

yyvalue yyGetRandomDouble() {
    double r = (double)rand() / RAND_MAX;
    return double_to_yyvalue(r);
}
