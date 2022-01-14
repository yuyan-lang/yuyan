// to be replaced by primop direclty in assembly
#include "../globalInclude.h"

yy_ptr yyIntEqTest(yy_ptr i1, yy_ptr i2){
    int64_t int1 = addr_to_int(i1);
    int64_t int2 = addr_to_int(i2);

    return bool_to_addr(int1== int2);
}
yy_ptr yyIntAdd(yy_ptr i1, yy_ptr i2){
    return int_to_addr(addr_to_int(i1)+ addr_to_int(i2));
}
yy_ptr yyIntSub(yy_ptr i1, yy_ptr i2){
    return int_to_addr(addr_to_int(i1)- addr_to_int(i2));
}

// https://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c/8257754#8257754
yy_ptr yyIntToString(yy_ptr i1){

    int64_t num = addr_to_int(i1);
    int64_t bufSize = snprintf( NULL, 0, "%lld", num ) + 1;
    char *strBuffer = GC_MALLOC(bufSize);
    snprintf(strBuffer, bufSize, "%lld", num);
    return string_to_addr(strBuffer);
}

yy_ptr yyStringToInt(yy_ptr i1){
    char *s = addr_to_string(i1);
    int64_t num = strtol(s, NULL, 10);
    return int_to_addr(num);
}