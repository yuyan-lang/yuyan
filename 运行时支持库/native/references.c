
#include "common_include.h"
#include "memory_verifier.h"


yyvalue yyNewRef(yyvalue value){
    yyvalue storage = yy_gcAllocateArray(1);
    yy_write_tuple(storage, 0, value);
    return storage;
}

yyvalue yyReadRef(yyvalue addr){
    return yy_read_tuple(addr, 0);
}

yyvalue yyWriteRef(yyvalue new_value, yyvalue addr){
    yy_write_tuple(addr, 0, new_value);
    return unit_to_yyvalue();
}

yyvalue yyNewRefArray(yyvalue value, yyvalue lengthAddr){
    int64_t length = yyvalue_to_int(lengthAddr);
    yyvalue storage = yy_gcAllocateArray(length);
    for (int i = 0; i < length; i++)
    {
        yy_write_tuple(storage, i, value);
    }
    return storage;
}

yyvalue yyReadRefArray(yyvalue addr, yyvalue indexAddr){
    int64_t index = yyvalue_to_int(indexAddr);
    return yy_read_tuple(addr, index);
}

yyvalue yyWriteRefArray(yyvalue new_value, yyvalue indexAddr, yyvalue refAddr){
    int64_t index = yyvalue_to_int(indexAddr);
    yy_write_tuple(refAddr, index, new_value);
    return unit_to_yyvalue();
}


yyvalue yyNewRefArrayGeneric(yyvalue lengthAddr){
    int64_t length = yyvalue_to_int(lengthAddr);
    yyvalue ret = yy_gcAllocateArray(length);
    // verify_yyvalue(ret, true, 0);
    return ret;
}


yyvalue yy_豫言不安全转换(yyvalue value){
    return value;
}