

#ifndef YY_FASTGC_H
#define YY_FASTGC_H

#include <stdlib.h>
#include <string.h>


void yy_fastgc_init(int64_t initial_size);

void* yy_fastgc_malloc(int64_t size);

void* yy_fastgc_realloc(void* ptr, int64_t size);

#endif /* YY_FASTGC_H */
