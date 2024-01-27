#include "common_include.h"
#include "garbage_collector.h"

/**
 * I decided to go with 128 bits (16 bytes) for each data. They arranged as follows:
 * 
 * Lower 64 bits (8 bytes): the actual data, integers, pointers, doubles.
 * 
 * 
 * Next 8 bits (1 byte): Type information. 
 *  0: empty value (unit / void) (data is all zero)
 *  1: pointer to tuples - length stores number of elements
 *  2: integers
 *  3: doubles
 *  4: strings (data is a pointer to char*) (static)  - length stores number of bytes INCLUDING the NULL terminator
 *  5: boolean
 *  6: pointers to functions
 *  7: pointers to static objects
 *  8: pointers to stack
 *  9: transfer addresses (used during GC)
 *  10: heap strings (data is a pointer to the heap)
 *  11: heap headers (only in the heap, mark the start of a string, data empty (due to endian issues)), length is the strlen (without null terminator)
 *  12: generic pointer that are generated in the runtime by reading e.g. stack_ptr or current_allocation_ptr
 *  13: constructor pointer (length is the constructor index, subtype is the tuple length)
 * 
 * 
 * Next 8 bits (1 byte): Subtype information
 * 
 * Next 32 bits (4 bytes): Length information.
 *  for tuples, it is the length of the tuple
 *  for strings, it is the byte length of the string excluding the trailing \0
 *  for constructor pointers, it is the index of the constructor (not the length) TODO: change this to length(maybe?) under the current setting, we do not allow constuctors with more than 256 elements, otherwise we do not allow constructors with index > 256, which is worse
 * 
 * Next 8 bits (1 byte): Reserved 
 * 
 * 
 * Next 7 bits (7/8 byte): Reserved for GC
 * Next 1 bit (1/8 byte): GC information
 * 
*/

const int type_empty_value = 0;
const int type_tuple = 1; // must point to dyn heap
const int type_int = 2;
const int type_double = 3;
const int type_static_string = 4;
const int type_boolean = 5;
const int type_pointer_to_function = 6;
const int type_pointer_to_static_object = 7;
const int type_pointer_to_stack = 8;
const int type_pointer_transfer_address = 9;
const int type_heap_string = 10; // 0x0A
const int type_heap_string_header = 11; // 0x0B
const int type_runtime_generic_ptr = 12; // 0x0C
const int type_constructor_tuple = 13;  // 0x0D

static int offset_type = 64;
static int offset_subtype = 72;
static int offset_length = 80;

typedef __uint128_t u128;
u128 to128literal(uint64_t arg)
{
    return (__uint128_t)arg;
}

uint64_t yyvalue_get_type(yyvalue arg) {
    return (arg >> offset_type) & 0xF;
}

uint64_t yyvalue_get_subtype(yyvalue arg) {
    return (arg >> offset_subtype) & 0xF;
}

bool yyvalue_is_heap_pointer(yyvalue arg) {
    uint64_t type = yyvalue_get_type(arg);
    return type == type_tuple || type == type_heap_string || type == type_constructor_tuple;
}

bool yyvalue_is_heap_string_pointer(yyvalue arg) {
    uint64_t type = yyvalue_get_type(arg);
    return type == type_heap_string;
}

bool yyvalue_is_tuple(yyvalue arg) {
    return yyvalue_get_type(arg) == type_tuple;
}

bool yyvalue_is_string_pointer(yyvalue arg) {
    return yyvalue_get_type(arg) == type_static_string || 
        yyvalue_get_type(arg) == type_heap_string;
}

bool yyvalue_is_heap_string_header(yyvalue arg) {
    return yyvalue_get_type(arg) == type_heap_string_header;

}

bool yyvalue_is_constructor_tuple(yyvalue arg) {
    return yyvalue_get_type(arg) == type_constructor_tuple;
}

// given string length (including the NULL terminator), return the number of allocation blocks (16 bytes each) needed 
uint64_t string_buffer_length_to_block_length(uint64_t length) {
    assert(sizeof(yyvalue) == 16);
    uint64_t remaining = length;
    uint64_t result = 1 + remaining / 16 + (remaining % 16 == 0 ? 0 : 1);
    return result;
}

yyvalue* yyvalue_to_heap_pointer(yyvalue arg) {
    assert(yyvalue_is_heap_pointer(arg));
    return (yyvalue*)(arg);
}

yyvalue heap_pointer_to_yyvalue(uint64_t type, uint64_t subtype, uint64_t raw_length, yyvalue* ptr) {
    assert(type == type_tuple || type == type_heap_string || type == type_constructor_tuple);
    yyvalue ret = (yyvalue)ptr;
    yyvalue_set_type(&ret, type);
    yyvalue_set_subtype(&ret, subtype);
    yyvalue_set_raw_length(&ret, raw_length);
    return ret;
}

uint64_t yyvalue_get_heap_pointer_length(yyvalue arg) {
    switch (yyvalue_get_type(arg))
    {
    case type_tuple:
        return yyvalue_to_tuple_length(arg);
        break;
    case type_heap_string:
        return yyvalue_to_heap_string_length(arg);
        break;
    case type_heap_string_header:
        return yyvalue_to_heap_string_length(arg);
        break;
    case type_constructor_tuple:
        return yyvalue_to_constructor_tuple_length(arg);
        break;

    default:
        errorAndAbort("yyvalue_get_heap_pointer_length: not a heap pointer");
        break;
    }
    return -1;
}

void yyvalue_set_type(yyvalue *arg, uint64_t type) {
    assert (type <= 0xF);
    u128 mask = to128literal(type) << offset_type;
    u128 mask_bits = to128literal(0xFF) << offset_type;
    *arg = (*arg & ~mask_bits) | mask;
}

void yyvalue_set_subtype(yyvalue *arg, uint64_t subtype) {
    assert (subtype <= 0xF);
    u128 mask = to128literal(subtype) << offset_subtype;
    u128 mask_bits = to128literal(0xFF) << offset_subtype;
    *arg = (*arg & ~mask_bits) | mask;
}

uint64_t yyvalue_get_raw_length(yyvalue arg) {
    return (arg >> offset_length) & 0xFFFFFFFF;
}

void yyvalue_set_raw_length(yyvalue *arg, uint64_t length) {
    u128 mask = to128literal(length) << offset_length;
    u128 mask_bits = to128literal(0xFFFFFFFF) << offset_length;
    *arg = (*arg & ~mask_bits) | mask;
}

uint64_t yyvalue_get_strlen(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_static_string
        || yyvalue_get_type(arg) == type_heap_string
        || yyvalue_get_type(arg) == type_heap_string_header);
    return yyvalue_get_raw_length(arg) - 1;
}


char * yyvalue_to_string(yyvalue arg) {
    switch (yyvalue_get_type(arg))
    {
    case type_static_string:
        return (char *)arg;
        break;
    case type_heap_string:
        return yyvalue_to_heap_string_pointer(arg);
        break;
    
    default:
        abort();
        break;
    }
}

int64_t yyvalue_to_int(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_int);
    return (int64_t) arg;
}

double  yyvalue_to_double(yyvalue arg) {
    assert(yyvalue_get_type(arg) == type_double);
    uint64_t lowerbits = (uint64_t)arg;
    return *(double *)&arg;
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
    return yyvalue_get_raw_length(arg);
}

uint64_t yyvalue_to_constructor_index(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_constructor_tuple);
    return yyvalue_get_raw_length(arg);
}

uint64_t yyvalue_to_constructor_tuple_length(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_constructor_tuple);
    return yyvalue_get_subtype(arg);
}


char* yyvalue_to_heap_string_pointer(yyvalue arg){
    assert(yyvalue_is_heap_string_pointer(arg));
    yyvalue* buffer_ptr = yyvalue_to_heap_pointer(arg);
    char *ret = (char *)(buffer_ptr + 1);
    return ret;
}

uint64_t yyvalue_to_heap_string_length(yyvalue arg){
    assert(yyvalue_get_type(arg) == type_heap_string || yyvalue_get_type(arg) == type_heap_string_header);
    uint64_t raw_length =  yyvalue_get_raw_length(arg);
    uint64_t actual_length = string_buffer_length_to_block_length(raw_length);
    return actual_length;
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

yyvalue static_string_to_yyvalue(const char * str){
    yyvalue ret = (yyvalue)str;
    yyvalue_set_type(&ret, type_static_string);
    yyvalue_set_raw_length(&ret, strlen(str) + 1);
    return ret;
}

// the length should include the null terminator
yyvalue yy_gcAllocateStringBuffer(uint64_t byte_length) {
    uint64_t actual_size = string_buffer_length_to_block_length(byte_length);
    yyvalue* buffer_ptr = yy_gc_malloc_array(actual_size);
    yyvalue header = 0;
    yyvalue_set_raw_length(&header, byte_length);
    yyvalue_set_type(&header, type_heap_string_header);
    buffer_ptr[0] = header;
    yyvalue ret = (yyvalue)buffer_ptr;
    yyvalue_set_type(&ret, type_heap_string);
    yyvalue_set_raw_length(&ret, byte_length);
    // fprintf(stderr, "Allocated String Buffer byte_length = " PRIu64 ", strlen = " PRIu64 ", yy_get_strlen = " PRIu64 "\n",
    //         byte_length, strlen(yyvalue_to_string(ret)), yyvalue_get_strlen(ret));
    return ret;
}

// byte length must contain \0
yyvalue malloc_string_to_yyvalue(uint64_t byte_length, const char * str){
    // assert(str[byte_length - 1] == '\0');
    assert(strlen(str) == byte_length - 1);
    yyvalue ret = yy_gcAllocateStringBuffer(byte_length);
    char* actual_str_ptr = yyvalue_to_heap_string_pointer(ret);
    memcpy(actual_str_ptr, str, byte_length);
    return ret;
}

yyvalue int_to_yyvalue(int64_t i){ 
    yyvalue ret = (yyvalue)i;
    yyvalue_set_type(&ret, type_int);
    return ret;
}

yyvalue double_to_yyvalue(double i){
    uint64_t lowerbits = *(uint64_t *)&i;
    yyvalue ret = (yyvalue)lowerbits;
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
    yyvalue_set_raw_length(&ret, length);
    return ret;
}

yyvalue tuple_to_yyvalue(uint64_t length, const yyvalue elems[]){
    yyvalue* returnStorage = (yyvalue*) yy_gc_malloc_array(length);
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

yyvalue array_to_iso_addr(uint64_t length, const yyvalue elems[]){
    yyvalue tup = tuple_to_yyvalue(length, elems);
    return tuple_to_yyvalue(2, (yyvalue[]){tup, int_to_yyvalue(length)});
}


yyvalue yy_read_tuple(yyvalue tuple, uint64_t index){
    yyvalue *tup = yyvalue_to_tuple(tuple);
    assert(0 <= index && index < yyvalue_to_tuple_length(tuple));
    return tup[index];
}

void yy_write_tuple(yyvalue tuple, uint64_t index, yyvalue value){
    yyvalue *tup = yyvalue_to_tuple(tuple);
    assert(0 <= index && index < yyvalue_to_tuple_length(tuple));
    tup[index] = value;
}

yyvalue yy_read_heap_pointer(yyvalue ptr, uint64_t index){
    yyvalue *tup = yyvalue_to_heap_pointer(ptr);
    uint64_t length = yyvalue_get_heap_pointer_length(ptr);
    assert(0 <= index && index < length);
    return tup[index];
}

void yy_write_heap_pointer(yyvalue ptr, uint64_t index, yyvalue value){
    yyvalue *tup = yyvalue_to_heap_pointer(ptr);
    assert(0 <= index && index < yyvalue_get_heap_pointer_length(ptr));
    tup[index] = value;
}