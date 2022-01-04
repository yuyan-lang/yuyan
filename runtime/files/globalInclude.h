#include <stdlib.h>
#include <stdio.h>
#include "gc.h" // https://hboehm.info/gc/ libgc 
#include <uv.h> 

extern int global_argc;
extern char** global_argv;

extern uv_loop_t *uv_global_loop;

extern uint64_t * allocateArray(uint64_t size);


uint64_t* unit_to_addr();
uint64_t* string_to_addr(const char * str);

uint64_t c_runtime_internal_error();
char * addr_to_string(uint64_t* arg);

uint64_t iso_list_get_length(const uint64_t * list) ;
uint64_t** iso_list_get_elements(const uint64_t * list);
uint64_t* tuple_to_addr(uint64_t length, const uint64_t* elems[]);