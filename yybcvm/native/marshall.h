
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include "type_defs.h"

// type and length
uint64_t yyvalue_get_type(yyvalue arg);
void yyvalue_set_type(yyvalue *arg, uint64_t type);
uint64_t yyvalue_get_subtype(yyvalue arg);
void yyvalue_set_subtype(yyvalue *arg, uint64_t subtype);
uint64_t yyvalue_get_raw_length(yyvalue arg);
void yyvalue_set_raw_length(yyvalue *arg, uint64_t length);
uint64_t yyvalue_get_strlen(yyvalue arg);


// read yyvalue
char *yyvalue_to_string(yyvalue arg);
int64_t yyvalue_to_int(yyvalue arg);
double yyvalue_to_double(yyvalue arg);
bool yyvalue_to_bool(yyvalue arg);
yyvalue *yyvalue_to_tuple(yyvalue arg);
uint64_t yyvalue_to_tuple_length(yyvalue arg);
uint64_t yyvalue_to_heap_string_length(yyvalue arg);
yy_function_type yyvalue_to_funcptr(yyvalue arg);
yyvalue *yyvalue_to_staticptr(yyvalue arg);
yyvalue *yyvalue_to_stackptr(yyvalue arg);
yyvalue *yyvalue_to_transfer_address(yyvalue arg);
void *yyvalue_to_generic_ptr(yyvalue arg);
yyvalue *yyvalue_to_heap_pointer(yyvalue arg);
uint64_t yyvalue_get_heap_pointer_length(yyvalue arg);
char *yyvalue_to_heap_string_pointer(yyvalue arg);
uint64_t yyvalue_to_constructor_tuple_length(yyvalue arg);
uint64_t yyvalue_to_constructor_tuple_idx(yyvalue arg);
yyvalue* yyvalue_to_constructor_tuple_tuple(yyvalue arg);

// testing type 
bool yyvalue_is_heap_pointer(yyvalue arg);
bool yyvalue_is_tuple(yyvalue arg);
bool yyvalue_is_heap_string_header(yyvalue arg);
bool yyvalue_is_string_pointer(yyvalue arg);
bool yyvalue_is_constructor_tuple(yyvalue arg);
uint64_t string_buffer_length_to_block_length(uint64_t length);

// write yyvalue
yyvalue unit_to_yyvalue();
yyvalue raw_heap_string_to_yyvalue(uint64_t bytelength, const char *str);
yyvalue malloc_string_to_yyvalue(uint64_t bytelength, const char *str);
yyvalue static_string_to_yyvalue(const char *str);
yyvalue int_to_yyvalue(int64_t i);
yyvalue double_to_yyvalue(double i);
yyvalue bool_to_yyvalue(bool b);
yyvalue funcptr_to_yyvalue(yy_function_type func);
yyvalue staticptr_to_yyvalue(yyvalue *ptr);
yyvalue stackptr_to_yyvalue(yyvalue *ptr);
yyvalue raw_tuple_to_yyvalue(uint64_t length, const yyvalue* elems);
yyvalue tuple_to_yyvalue(uint64_t length, const yyvalue elems[]);
yyvalue transfer_address_to_yyvalue(yyvalue *transfer_address);
yyvalue heap_pointer_to_yyvalue(uint64_t type, uint64_t subtype, uint64_t raw_length, yyvalue *ptr);
yyvalue runtime_heap_pointer_to_yyvalue(yyvalue *ptr);
yyvalue raw_constructor_tuple_to_yyvalue(uint64_t idx, uint64_t length, const yyvalue *elems);
yyvalue constructor_tuple_to_yyvalue(uint64_t idx, uint64_t length, const yyvalue elems[]);

// list ops
uint64_t iso_list_get_length(const yyvalue list);
yyvalue *iso_list_get_elements(const yyvalue list);
yyvalue array_to_iso_addr(uint64_t length, const yyvalue elems[]);

// tuple ops
yyvalue yy_read_tuple(yyvalue tuple, uint64_t index);
void yy_write_tuple(yyvalue tuple, uint64_t index, yyvalue value);
yyvalue yy_read_heap_pointer(yyvalue ptr, uint64_t index);
void yy_write_heap_pointer(yyvalue ptr, uint64_t index, yyvalue value);
