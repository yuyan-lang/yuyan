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


extern int global_argc;
extern char** global_argv;

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

yyvalue yy_gcAllocateArray(uint64_t size);
void *yy_gcAllocateBytes(uint64_t size);
yyvalue yy_gcAllocateStringBuffer(uint64_t length);
void *yy_gcReallocateBytes(void* ptr, uint64_t old_size, uint64_t new_size);


// runtime configurations
extern int64_t use_libgc;
int64_t yy_runtime_start();

void start_yy_profiler();
extern bool use_profiler;



extern yyvalue* stack_ptr;
extern yyvalue* stack;
extern yy_function_type current_function;
extern pthread_mutex_t stack_ptr_mutex;
extern yyvalue entryMain(yyvalue, yyvalue, yyvalue, yyvalue);

void yy_perform_gc(yyvalue* additional_root_point);
void verify_gc(yyvalue* additional_root_point);
void yy_gc_init();
extern bool during_gc;