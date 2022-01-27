#include "../globalInclude.h"

yy_ptr yyGetRandomNumber(yy_ptr upperBoundPtr) {
    uint64_t result = 0;
    uint64_t upperBoundInt = addr_to_int(upperBoundPtr);
    uv_random_t req;
    if (upperBoundInt > 0)
    {
        char buffer[8];
        int result = uv_random(uv_global_loop, &req, &buffer, 8, 0,NULL );
        if (result != 0){
            throwException("Error: Could not get random number in libuv.\n");
        }
        else {
            result = *(uint64_t*)buffer;
            result = result % upperBoundInt;
        }
    }
    int64_t resultInt = (int64_t) result;
    if(resultInt < 0){
        resultInt = -resultInt;
    }
    return int_to_addr(resultInt);
}