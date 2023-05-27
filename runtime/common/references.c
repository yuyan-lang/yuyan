
#include "common_include.h"


yy_ptr yyNewRef(yy_ptr value){
    yy_ptr storage = allocateAndSetHeader(9, 1);
    storage[0] = addr_to_data(value);
    return storage;
}

yy_ptr yyReadRef(yy_ptr addr){
    return data_to_addr(addr[0]);
}

yy_ptr yyWriteRef(yy_ptr new_value, yy_ptr addr){
    addr[0] = addr_to_data(new_value);
    return unit_to_addr();
}

yy_ptr yyNewRefArray(yy_ptr value, yy_ptr lengthAddr){
    int64_t length = addr_to_int(lengthAddr);
    yy_ptr *storage = yy_gcAllocateBytes(length * sizeof (void*));
    for (int i = 0; i < length; i++)
    {
        storage[i] = value;
    }
    return (yy_ptr)storage;
}

yy_ptr yyReadRefArray(yy_ptr addr, yy_ptr indexAddr){

    yy_ptr *storage = (yy_ptr *)(addr);
    int64_t index = addr_to_int(indexAddr);
    return storage[index];
}

yy_ptr yyWriteRefArray(yy_ptr new_value, yy_ptr indexAddr, yy_ptr refAddr){
    int64_t index = addr_to_int(indexAddr);
    yy_ptr *storage =(yy_ptr *) (refAddr);
    storage[index] = (new_value);
    return unit_to_addr();
}


yy_ptr yyNewRefArrayGeneric(yy_ptr lengthAddr){
    int64_t length = addr_to_int(lengthAddr);
    yy_ptr *storage = yy_gcAllocateBytes(length * sizeof (void*));
    return (yy_ptr)storage;
}


yy_ptr yy_豫言不安全转换(yy_ptr value){
    return value;
}