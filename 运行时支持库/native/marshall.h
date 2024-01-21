
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
typedef __uint128_t yyvalue;

extern int type_empty_value;
extern int type_tuple;
extern int type_int;
extern int type_double;
extern int type_string;
extern int type_boolean;
extern int type_pointer_to_function;
extern int type_pointer_to_static_object;

extern int offset_type;
extern int offset_length;


// type and length
uint64_t get_yyvalue_type(yyvalue arg);
void set_yyvalue_type(yyvalue *arg, uint64_t type);
uint64_t get_yyvalue_length(yyvalue arg);
void set_yyvalue_length(yyvalue *arg, uint64_t length);


// read yyvalue
char *yyvalue_to_string(yyvalue arg);
int64_t yyvalue_to_int(yyvalue arg);
double yyvalue_to_double(yyvalue arg);
bool yyvalue_to_bool(yyvalue arg);
yyvalue *yyvalue_to_tuple(yyvalue arg);
uint64_t yyvalue_to_tuple_length(yyvalue arg);
yy_function_type yyvalue_to_funcptr(yyvalue arg);
yyvalue *yyvalue_to_staticptr(yyvalue arg);
yyvalue *yyvalue_to_stackptr(yyvalue arg);
void *yyvalue_to_generic_ptr(yyvalue arg);

// write yyvalue
yyvalue unit_to_yyvalue();
yyvalue string_to_yyvalue(const char *str);
yyvalue int_to_yyvalue(int64_t i);
yyvalue double_to_yyvalue(double i);
yyvalue bool_to_yyvalue(bool b);
yyvalue funcptr_to_yyvalue(yy_function_type func);
yyvalue staticptr_to_yyvalue(yyvalue *ptr);
yyvalue stackptr_to_yyvalue(yyvalue *ptr);
yyvalue raw_tuple_to_yyvalue(uint64_t length, const yyvalue* elems);
yyvalue tuple_to_yyvalue(uint64_t length, const yyvalue elems[]);

// list ops
uint64_t iso_list_get_length(const yyvalue list);
yyvalue *iso_list_get_elements(const yyvalue list);
yyvalue heap_array_to_yyvalue(uint64_t length, const yyvalue *elems);
yyvalue array_to_iso_addr(uint64_t length, const yyvalue elems[]);

// tuple ops
yyvalue yy_read_tuple(yyvalue tuple, uint64_t index);
void yy_write_tuple(yyvalue tuple, uint64_t index, yyvalue value);