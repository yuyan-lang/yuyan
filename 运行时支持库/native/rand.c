#include "common_include.h"

yy_ptr yyGetRandomInt(yy_ptr upperBoundPtr) {
    uint64_t upperBoundInt = addr_to_int(upperBoundPtr);
    return int_to_addr(rand() % upperBoundInt);
    // uint64_t result = 0;
    // uint64_t upperBoundInt = addr_to_int(upperBoundPtr);
    // uv_random_t req;
    // if (upperBoundInt > 0)
    // {
    //     char buffer[8];
    //     int result = uv_random(uv_global_loop, &req, &buffer, 8, 0,NULL );
    //     if (result != 0){
    //         errorAndAbort("Error: Could not get random number in libuv.\n");
    //     }
    //     else {
    //         result = *(uint64_t*)buffer;
    //         result = result % upperBoundInt;
    //     }
    // }
    // int64_t resultInt = (int64_t) result;
    // if(resultInt < 0){
    //     resultInt = -resultInt;
    // }
    // return int_to_addr(resultInt);
}

yy_ptr yyGetRandomDouble() {
    double r = (double)rand() / RAND_MAX;
    return double_to_addr(r);
}
