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
 *  5: boolean
 *  6: pointers to functions
 *  7: pointers to static objects
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

int type_empty_value = 0;
int type_tuple = 1; // must point to dyn heap
int type_int = 2;
int type_double = 3;
int type_string = 4;
int type_boolean = 5;
int type_pointer_to_function = 6;
int type_pointer_to_static_object = 7;
int type_pointer_to_stack = 8;
int type_pointer_transfer_address = 9;

static int offset_type = 64;
static int offset_length = 80;

typedef __uint128_t u128;
u128 to128literal(uint64_t arg)
{
    return (__uint128_t)arg;
}

uint64_t yyvalue_get_type(yyvalue arg) {
    return (arg >> offset_type) & 0xF;
}

void yyvalue_set_type(yyvalue *arg, uint64_t type) {
    u128 mask = to128literal(type) << offset_type;
    u128 mask_bits = to128literal(0xF) << offset_type;
    *arg = (*arg & ~mask_bits) | mask;
}

uint64_t yyvalue_get_length(yyvalue arg) {
    return (arg >> offset_length) & 0xFFFFFFFF;
}

void yyvalue_set_length(yyvalue *arg, uint64_t length) {
    u128 mask = to128literal(length) << offset_length;
    u128 mask_bits = to128literal(0xFFFFFFFF) << offset_length;
    *arg = (*arg & ~mask_bits) | mask;
}

uint64_t yyvalue_get_strlen(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_string);
    return yyvalue_get_length(arg);
}


char * yyvalue_to_string(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_string);
    return (char *)arg;
}

int64_t yyvalue_to_int(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_int);
    return (int64_t) arg;
}

double  yyvalue_to_double(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_double);
    return (double) arg;
}

bool yyvalue_to_bool(yyvalue arg){
    assert (yyvalue_get_type(arg) == type_boolean);
    return (bool)&arg;
}

yyvalue* yyvalue_to_tuple(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_tuple);
    return (yyvalue *)arg;
}

uint64_t yyvalue_to_tuple_length(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_tuple);
    return yyvalue_get_length(arg);
}

yy_function_type yyvalue_to_funcptr(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_pointer_to_function);
    return (yy_function_type)arg;
}

yyvalue* yyvalue_to_staticptr(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_pointer_to_static_object);
    return (yyvalue*)arg;
}

yyvalue* yyvalue_to_stackptr(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_pointer_to_stack);
    return (yyvalue*)arg;
}

yyvalue* yyvalue_to_transfer_address(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_pointer_transfer_address);
    return (yyvalue*)arg;
}

void* yyvalue_to_generic_ptr(yyvalue arg) {
    return (void*)arg;
}


yyvalue unit_to_yyvalue(){
    return 0;
}

yyvalue string_to_yyvalue(const char * str){
    yyvalue ret = (yyvalue)str;
    yyvalue_set_type(&ret, type_string);
    yyvalue_set_length(&ret, strlen(str));
    return ret;
}

yyvalue int_to_yyvalue(int64_t i){
    yyvalue ret = (yyvalue)i;
    yyvalue_set_type(&ret, type_int);
    return ret;
}

yyvalue double_to_yyvalue(double i){
    yyvalue ret = (yyvalue)i;
    yyvalue_set_type(&ret, type_double);
    return ret;
}


yyvalue funcptr_to_yyvalue(yy_function_type func) {
    yyvalue ret = (yyvalue)func;
    yyvalue_set_type(&ret, type_pointer_to_function);
    return ret;
}

yyvalue staticptr_to_yyvalue(yyvalue* static_ptr) {
    yyvalue ret = (yyvalue)static_ptr;
    yyvalue_set_type(&ret, type_pointer_to_static_object);
    return ret;
}

yyvalue stackptr_to_yyvalue(yyvalue* stackptr) {
    yyvalue ret = (yyvalue)stackptr;
    yyvalue_set_type(&ret, type_pointer_to_stack);
    return ret;
}

yyvalue transfer_address_to_yyvalue(yyvalue* transfer_address) {
    yyvalue ret = (yyvalue)transfer_address;
    yyvalue_set_type(&ret, type_pointer_transfer_address);
    return ret;
}

yyvalue bool_to_yyvalue(bool b){
    yyvalue ret = (yyvalue)b;
    yyvalue_set_type(&ret, type_boolean);
    return ret;
}

yyvalue raw_tuple_to_yyvalue(uint64_t length, const yyvalue* elems){
    yyvalue ret = (yyvalue)elems;
    yyvalue_set_type(&ret, type_tuple);
    yyvalue_set_length(&ret, length);
    return ret;
}

yyvalue tuple_to_yyvalue(uint64_t length, const yyvalue elems[]){
    yyvalue* returnStorage = (yyvalue*) yy_gcAllocateBytes(length * sizeof(yyvalue));
    for (int i = 0; i < length; i ++){
        returnStorage[i] = elems[i];
    }
    return raw_tuple_to_yyvalue(length, returnStorage);
}

uint64_t iso_list_get_length(const yyvalue list) {
    yyvalue *tups = yyvalue_to_tuple(list); 
    return yyvalue_to_int(tups[1]);
}

yyvalue* iso_list_get_elements(const yyvalue list) {
    yyvalue *tups = yyvalue_to_tuple(list);
    return (yyvalue_to_tuple((tups[0])));
}

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


yyvalue yy_read_tuple(yyvalue tuple, uint64_t index){
    yyvalue *tup = yyvalue_to_tuple(tuple);
    assert(0 <= index && index < yyvalue_to_tuple_length(tuple));
    return tup[index];
}

void yy_write_tuple(yyvalue tuple, uint64_t index, yyvalue value){
    yyvalue *tup = yyvalue_to_tuple(tuple);
    assert(0 <= index && index <= yyvalue_to_tuple_length(tuple));
    tup[index] = value;
}