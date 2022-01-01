#include "globalInclude.h"

char * addr_to_string(uint64_t* arg) {
    return (char *)arg[1];
}

uint64_t* unit_to_addr(){
    uint64_t* returnStorage = allocateArray(1);
    uint64_t header =  5 ;
    header= header<< (62 - 6);
    *returnStorage = header;
    return returnStorage;
}

uint64_t* string_to_addr(const char * str){
    uint64_t* returnStorage = allocateArray(2);
    uint64_t header =  6 ;
    header= header<< (62 - 6);
    *returnStorage = header;
    returnStorage[1] = (uint64_t ) str;
    return returnStorage;
}