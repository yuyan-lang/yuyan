#include <stdlib.h>
#include <stdio.h>
#include "gc.h" // https://hboehm.info/gc/ libgc 
#include <uv.h> 
#include <stdbool.h>

typedef uint64_t* yy_ptr;

extern int global_argc;
extern char** global_argv;

extern uv_loop_t *uv_global_loop;

extern yy_ptr allocateArray(uint64_t size);


yy_ptr unit_to_addr();
yy_ptr string_to_addr(const char * str);

uint64_t c_runtime_internal_error();
char * addr_to_string(yy_ptr arg);

uint64_t iso_list_get_length(const yy_ptr list) ;
yy_ptr* iso_list_get_elements(const yy_ptr list);
yy_ptr tuple_to_addr(uint64_t length, const yy_ptr elems[]);
yy_ptr bool_to_addr(bool b);


void readStreamUntilEofIntoDataAync(uv_stream_t *stream);