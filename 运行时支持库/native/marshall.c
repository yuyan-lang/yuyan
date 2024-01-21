#include "common_include.h"

/**
 * I decided to go with 128 bits (16 bytes) for each data. They arranged as follows:
 * 
 * Lower 64 bits (8 bytes): the actual data, integers, pointers, doubles.
 * 
 * 
 * Next 8 bits (1 byte): Type information. 
 *  0: empty value (unit / void) (data is all zero)
 *  1: pointer to tuples
 *  2: integers
 *  3: doubles
 *  4: strings (data is a pointer to char*)
 *  5: pointers
 *  6: boolean
 * 
 * Next 8 bits (1 byte): Reserved
 * 
 * Next 32 bits (4 bytes): Length information.
 *  for tuples, it is the length of the tuple
 *  for strings, it is the byte length of the string excluding the trailing \0
 * 
 * Next 8 bits (1 byte): Reserved 
 * 
 * 
 * Next 7 bits (7/8 byte): Reserved for GC
 * Next 1 bit (1/8 byte): GC information
 * 
*/

static int type_empty_value = 0;
static int type_tuple = 1;
static int type_int = 2;
static int type_double = 3;
static int type_string = 4;
static int type_pointer = 5;
static int type_boolean = 6;

static int offset_type = 64;
static int offset_length = 80;

typedef __uint128_t u128;
u128 to128literal(uint64_t arg)
{
    return (__uint128_t)arg;
}

uint64_t get_yyvalue_type(yyvalue arg) {
    return (arg >> offset_type) & 0xF;
}

void set_yyvalue_type(yyvalue *arg, uint64_t type) {
    u128 mask = to128literal(type) << offset_type;
    u128 mask_bits = to128literal(0xF) << offset_type;
    *arg = (*arg & ~mask_bits) | mask;
}

uint64_t get_yyvalue_length(yyvalue arg) {
    return (arg >> offset_length) & 0xFFFFFFFF;
}

void set_yyvalue_length(yyvalue *arg, uint64_t length) {
    u128 mask = to128literal(length) << offset_length;
    u128 mask_bits = to128literal(0xFFFFFFFF) << offset_length;
    *arg = (*arg & ~mask_bits) | mask;
}


char * yyvalue_to_string(yyvalue arg) {
    assert(get_yyvalue_type(arg) == type_string);
    return (char *)arg;
}

int64_t yyvalue_to_int(yyvalue arg) {
    assert(get_yyvalue_type(arg) == type_int);
    return (int64_t) arg;
}

double  yyvalue_to_double(yyvalue arg) {
    assert(get_yyvalue_type(arg) == type_double);
    return (double) arg;
}

bool yyvalue_to_bool(yyvalue arg){
    assert (get_yyvalue_type(arg) == type_boolean);
    return (bool)&arg;
}

yyvalue* yyvalue_to_tuple(yyvalue arg){
    return (yyvalue *)arg;
}


yyvalue unit_to_yyvalue(){
    return 0;
}

yyvalue string_to_yyvalue(const char * str){
    yyvalue ret = (yyvalue)str;
    set_yyvalue_type(&ret, type_string);
    return ret;
}

yyvalue int_to_yyvalue(int64_t i){
    yyvalue ret = (yyvalue)i;
    set_yyvalue_type(&ret, type_int);
    return ret;
}

yyvalue double_to_yyvalue(double i){
    yyvalue ret = (yyvalue)i;
    set_yyvalue_type(&ret, type_double);
    return ret;
}


yyvalue bool_to_yyvalue(bool b){
    yyvalue ret = (yyvalue)b;
    set_yyvalue_type(&ret, type_boolean);
    return ret;
}

yyvalue tuple_to_yyvalue(uint64_t length, const yyvalue elems[]){

    yyvalue returnStorage = yy_gcAllocateArray(length);
    for (int i = 0; i < length; i ++){
        returnStorage[i] = (uint64_t) elems[i];
    }
    return returnStorage;
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_length_rec(const yyvalue list, uint64_t acc) {
    // the type is a fold on the outer left
    // yyvalue unfolded = (yyvalue)list[1];
    // then it is a tuple, the label's position is stored in index 1
    uint64_t labelIndex = list[0];
    if (labelIndex == 1) {
        return acc;
    } else if (labelIndex == 2){
        // OLD 0: header 1: index 2: unit (type) 3: current element 4: next element
        // NOW:  0: index 1: unit (type) 2: current element 3: next element
        yyvalue next = data_to_yyvalue(list[3]);
        return iso_list_length_rec(next, acc+1);
    } else {
        errorAndAbort("iso_list_get_length error");
        return -1;
    }
}

// gets the length of a list of type rho t. (0 : 1) + (1 : a x t)
uint64_t iso_list_get_length(const yyvalue list) {
    yyvalue *tups = yyvalue_to_tuple(list); // the tuple in c runtime is differnt from yuyanlang 0.2 tuple, which is an iterative linked list
    return yyvalue_to_int(tups[1]);
}

// gets the elements of a list of type rho t. (0 : 1) + (1 : a x t)
yyvalue* iso_list_get_elements(const yyvalue list) {
    // uint64_t size = iso_list_get_length(list);

    // yyvalue* result = (yyvalue*)yy_gcAllocateArray(size);

    // uint64_t curIndex = 0;
    // yyvalue curList = list;

    // while(curIndex < size) {
    //     result[curIndex] = data_to_yyvalue(curList[2]);
    //     curList = data_to_yyvalue(curList[3]);
    //     curIndex += 1;
    // }
    yyvalue *tups = yyvalue_to_tuple(list);
    return (yyvalue_to_tuple((tups[0])));
}

// const char* iso_list_nil_label = "一";
// const char* iso_list_cons_label = "二";

// yyvalue fold_to_yyvalue(yyvalue toFold) {
//     yyvalue result = allocateAndSetHeader(2, 1);
//     result[1] = yyvalue_to_data(toFold);
//     return result;
// }
yyvalue function_to_yyvalue(void* func) {
    yyvalue result = allocateAndSetHeader(1, 1);
    result[0] = yyvalue_to_data(func);
    return result;
}

// yyvalue iso_list_nil_to_yyvalue() {
//     // the unit here is the implicit type argument
//     yyvalue tps[] = {unit_to_yyvalue(0), int_to_yyvalue(1)};
//     return tuple_to_yyvalue(2, tps);
// }
// yyvalue iso_list_cons_to_yyvalue(yyvalue elem, yyvalue rest){
//     yyvalue tps[] = {int_to_yyvalue(2), unit_to_yyvalue(), elem, rest};
//     return tuple_to_yyvalue(4, tps);
// }

yyvalue heap_array_to_yyvalue(uint64_t length, const yyvalue* elems){

    yyvalue len = int_to_yyvalue(length);

    yyvalue res_tup[] = {(yyvalue)elems, len};
    yyvalue res = tuple_to_yyvalue(2, res_tup);

    return res;
}

yyvalue array_to_iso_addr(uint64_t length, const yyvalue elems[]){

    yyvalue tup = tuple_to_yyvalue(length, elems);

    return heap_array_to_yyvalue(length, (yyvalue *)tup);
}



