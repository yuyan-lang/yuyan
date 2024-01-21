
#include "common_include.h"


yyvalue yyNewRef(yyvalue value){
    yyvalue storage = allocateAndSetHeader(9, 1);
    storage[0] = addr_to_data(value);
    return storage;
}

yyvalue yyReadRef(yyvalue addr){
    return data_to_addr(addr[0]);
}

yyvalue yyWriteRef(yyvalue new_value, yyvalue addr){
    addr[0] = addr_to_data(new_value);
    return unit_to_addr();
}

yyvalue yyNewRefArray(yyvalue value, yyvalue lengthAddr){
    int64_t length = addr_to_int(lengthAddr);
    yyvalue *storage = yy_gcAllocateBytes(length * sizeof (void*));
    for (int i = 0; i < length; i++)
    {
        storage[i] = value;
    }
    return (yyvalue)storage;
}

yyvalue yyReadRefArray(yyvalue addr, yyvalue indexAddr){

    yyvalue *storage = (yyvalue *)(addr);
    int64_t index = addr_to_int(indexAddr);
    return storage[index];
}

yyvalue yyWriteRefArray(yyvalue new_value, yyvalue indexAddr, yyvalue refAddr){
    int64_t index = addr_to_int(indexAddr);
    yyvalue *storage =(yyvalue *) (refAddr);
    storage[index] = (new_value);
    return unit_to_addr();
}


yyvalue yyNewRefArrayGeneric(yyvalue lengthAddr){
    int64_t length = addr_to_int(lengthAddr);
    yyvalue *storage = yy_gcAllocateBytes(length * sizeof (void*));
    return (yyvalue)storage;
}


yyvalue yy_豫言不安全转换(yyvalue value){
    return value;
}