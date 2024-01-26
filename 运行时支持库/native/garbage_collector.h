

#ifndef YY_GC_H
#define YY_GC_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "common_include.h"

extern yyvalue * gc_root_points[];
extern int gc_root_points_count;

extern yyvalue* current_heap;
extern yyvalue* new_heap;
extern yyvalue *current_allocation_ptr;
extern yyvalue *current_heap_gc_limit;
extern bool should_expand_heap;

void yy_gc_init();

void yy_register_gc_rootpoint(yyvalue* ptr);

void* yy_gc_malloc_array(uint64_t size);

void* yy_gc_realloc_bytes(void* ptr, uint64_t old_size, uint64_t new_size);
void yy_perform_gc();

void verify_yyvalue(yyvalue arg, bool recursive, int depth);
void verify_yyvalue_new_heap(yyvalue arg, bool recursive, int depth);

bool is_an_old_pointer(yyvalue arg);
bool is_a_new_pointer(yyvalue arg);

// #ifndef TINY_HEAP_SIZE
// #define TINY_HEAP_SIZE (2 * 1024 * 1024)  // 32 MB
// #endif
#endif /* YY_FASTGC_H */
