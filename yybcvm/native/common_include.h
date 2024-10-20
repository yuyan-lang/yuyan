#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include "stdint.h"
#include "stdbool.h"
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <pthread.h>
#include "gc.h" // https://hboehm.info/gc/ libgc 
#include <uv.h> 
#include <assert.h>
// #define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include "time.h"

#ifdef __linux__
    #include <bsd/string.h>
#endif
#include "gc.h" // https://hboehm.info/gc/ libgc 
#include <uv.h> 

#include "marshall.h"
#include "debug_print.h"


extern int global_argc;
extern char** global_argv;
extern char *main_bytecode_filename;

extern uv_loop_t *uv_global_loop;

void readStreamUntilEofIntoDataAync(uv_stream_t *stream);


yyvalue unit_to_yyvalue();

uint64_t c_runtime_internal_error();
uint64_t errorAndAbort(char *errMsg);
char *yyvalue_to_string(yyvalue arg);

yyvalue allocateAndSetHeader(uint64_t type, uint64_t length);

uint64_t iso_list_get_length(const yyvalue list) ;
yyvalue* iso_list_get_elements(const yyvalue list);
yyvalue tuple_to_yyvalue(uint64_t length, const yyvalue elems[]);
yyvalue bool_to_yyvalue(bool b);
yyvalue array_to_iso_addr(uint64_t length, const yyvalue elems[]);

int64_t yyvalue_to_int(yyvalue arg);
double yyvalue_to_double(yyvalue arg);
yyvalue *yyvalue_to_tuple(yyvalue arg);
uint64_t yyvalue_to_tuple_length(yyvalue arg);
yyvalue int_to_yyvalue(int64_t i);
yyvalue double_to_yyvalue(double i);
// yyvalue function_to_yyvalue(void *func);


void yy_豫言初始化全局异常处理器();

yyvalue yy_gcAllocateTuple(uint64_t size);
yyvalue yy_gcAllocateStringBuffer(uint64_t length);


// runtime configurations
int64_t yy_runtime_start();

void start_yy_profiler();
extern bool use_profiler;




extern yyvalue* stack_start;
extern yyvalue* stack_end;
extern yyvalue* stack_ptr;
extern yyvalue* highest_stack_ptr;
extern yy_function_type current_function;
extern pthread_mutex_t stack_ptr_mutex;
extern void entryMain(yyvalue stack_top, yyvalue current_allocation_arg);

void yy_perform_gc();
void verify_gc();
void yy_gc_init();
extern bool during_gc;

void initialize_runtime_stack();
void yy_pre_function_call_gc();


#ifndef ENUM_YYVALUE_TYPE
#define ENUM_YYVALUE_TYPE

typedef enum {
    type_empty_value = 0,
    type_tuple = 1, // must point to dyn heap
    type_int = 2,
    type_double = 3,
    type_static_string = 4,
    type_boolean = 5,
    type_pointer_to_function = 6,
    type_pointer_to_static_object = 7,
    type_pointer_to_stack = 8,
    type_pointer_transfer_address = 9,
    type_heap_string = 10, // 0x0A
    type_heap_string_header = 11, // 0x0B
    type_runtime_generic_ptr = 12, // 0x0C
    type_constructor_tuple = 13,  // 0x0D
    type_pointer_to_c_stack = 14,  // 0x0E // used only in the runtime, should not appear at all on the stack
} YYValueType;

#endif