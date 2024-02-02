
#ifndef DEBUG_PRINT_H
#define DEBUG_PRINT_H

#include "common_include.h"
void yy_print_yyvalue(yyvalue v, uint64_t depth);
void yy_debug_log_print_args(uint64_t num, const char *func_name, ...);


#define DEBUG_LOG(num, ...)                                        \
    do                                                        \
    {                                                         \
        yy_debug_log_print_args(num, __func__, __VA_ARGS__); \
    } while (0)
#endif