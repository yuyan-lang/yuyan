// to be replaced by primop direclty in assembly
#include "common_include.h"

yyvalue yyIntEqTest(yyvalue i1, yyvalue i2){
    int64_t int1 = addr_to_int(i1);
    int64_t int2 = addr_to_int(i2);

    return bool_to_addr(int1== int2);
}
yyvalue yyIntAdd(yyvalue i1, yyvalue i2){
    return int_to_addr(addr_to_int(i1)+ addr_to_int(i2));
}
yyvalue yyIntSub(yyvalue i1, yyvalue i2){
    return int_to_addr(addr_to_int(i1)- addr_to_int(i2));
}
yyvalue yyIntMult(yyvalue i1, yyvalue i2){
    return int_to_addr(addr_to_int(i1) * addr_to_int(i2));
}
yyvalue yyIntDiv(yyvalue i1, yyvalue i2){
    return int_to_addr(addr_to_int(i1) / addr_to_int(i2));
}


// https://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c/8257754#8257754
yyvalue yyIntToString(yyvalue i1){

    int64_t num = addr_to_int(i1);
    int64_t bufSize = snprintf( NULL, 0, "%lld", (long long )num ) + 1;
    char *strBuffer = yy_gcAllocateBytes(bufSize);
    snprintf(strBuffer, bufSize, "%lld", (long long) num);
    return string_to_addr(strBuffer);
}

yyvalue yyDoubleAdd(yyvalue i1, yyvalue i2){
    return double_to_addr(addr_to_double(i1)+ addr_to_double(i2));
}
yyvalue yyDoubleSub(yyvalue i1, yyvalue i2){
    return double_to_addr(addr_to_double(i1)- addr_to_double(i2));
}
yyvalue yyDoubleMult(yyvalue i1, yyvalue i2){
    return double_to_addr(addr_to_double(i1) * addr_to_double(i2));
}
yyvalue yyDoubleDiv(yyvalue i1, yyvalue i2){
    return double_to_addr(addr_to_double(i1) / addr_to_double(i2));
}

// https://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c/8257754#8257754
yyvalue yyDoubleToString(yyvalue i1){

    double num = addr_to_double(i1);
    int64_t bufSize = snprintf( NULL, 0, "%lf", num ) + 1;
    char *strBuffer = yy_gcAllocateBytes(bufSize);
    snprintf(strBuffer, bufSize, "%lf", num);
    return string_to_addr(strBuffer);
}

yyvalue yyDoubleToInt(yyvalue d) {
    return int_to_addr((int64_t)addr_to_double(d));
}

yyvalue yyIntToDouble(yyvalue i) {
    double value = (double)addr_to_int(i);
    return double_to_addr(value);
}

yyvalue yyStringToInt(yyvalue i1){
    char *s = addr_to_string(i1);
    int64_t num = strtol(s, NULL, 10);
    return int_to_addr(num);
}

yyvalue yyStringToDouble(yyvalue i1){
    char *s = addr_to_string(i1);
    double num = strtod(s, NULL);
    return double_to_addr(num);
}
