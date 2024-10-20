#include "common_include.h"


// void yy_debug_log_print_args(uint64_t num, const char *func_name, ...) {
//     va_list args;
//     va_start(args, func_name);
//     fprintf(stderr, "%s called with ", func_name);

//     yyvalue arg;
//     arg = va_arg(args, yyvalue);
//     fprintf(stderr, "ret = ");
//     yy_print_yyvalue(arg, 0);
//     fprintf(stderr, ", ");
    
//     for (int i = 1; i < num; i++) {
//         arg = va_arg(args, yyvalue);
//         fprintf(stderr, "arg %d = ", i);
//         yy_print_yyvalue(arg, 0);
//         if (i != num - 1) {
//             fprintf(stderr, ", ");
//         }
//         va_end(args);
//     }

//     fprintf(stderr, "\n");
// }
