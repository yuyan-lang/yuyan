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
yy_ptr iso_list_nil_to_addr();
yy_ptr iso_list_cons_to_addr(yy_ptr elem, yy_ptr rest);
yy_ptr array_to_iso_addr(uint64_t length, const yy_ptr elems[]);

int64_t addr_to_int(yy_ptr arg);
double addr_to_double(yy_ptr arg);
yy_ptr int_to_addr(int64_t i);
yy_ptr double_to_addr(double i);
// type conversion function
yy_ptr data_to_addr(uint64_t elem);
uint64_t addr_to_data(yy_ptr ptr);

void readStreamUntilEofIntoDataAync(uv_stream_t *stream);
int informResultRec(FILE * file, yy_ptr result, int prevPred);
int informResult(yy_ptr result);