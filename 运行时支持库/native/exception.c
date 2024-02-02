

#include "common_include.h"
#include "memory_verifier.h"


int internalError(){
    printf("An Internal Error has occurred. The program is likely \
    to be compiled incorrectly. Type Checking perhaps is not sound.\n");
    exit(1);
    return -1;
}

uint64_t c_runtime_internal_error(){
    fprintf(stderr, "An Internal Error has occurred. The runtime library is likely \
    to be written incorrectly. Type Checking perhaps is not sound.\n");
    exit(1);
    return -1;
}

uint64_t errorAndAbort(char* errMsg){
    
    fprintf(stderr, "%s\n", errMsg);
    exit(1);
    return -1;
}

uint64_t errorAndAbort_yyrt(yyvalue errMsg){
    
    fprintf(stderr, "%s\n", yyvalue_to_string(errMsg));
    exit(1);
    return -1;
}

// I believe abssingle has two arguments , the first is just the closure itself
void 全局异常处理器(yyvalue stack_top, yyvalue current_allocation_arg){
    yyvalue argument = yyvalue_to_stackptr(stack_top)[3];
    fprintf(stderr, "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n豫言运行环境(yy_runtime)：未捕捉的异常(Uncaught Exception)：\n");
    fprintf(stderr, "尝试打印值：（可能会出现 异常）：\n");
    fflush(stderr);
    fprintf(stderr, "%s", yyvalue_to_string(yy_read_tuple(argument, 1)));
    fprintf(stderr, "\n");
    exit(1);
}


yyvalue 当前异常处理器;


void yy_豫言初始化全局异常处理器(){
    当前异常处理器 = tuple_to_yyvalue(2, (yyvalue[]){
        funcptr_to_yyvalue(全局异常处理器), unit_to_yyvalue() });
    yy_register_gc_rootpoint(&当前异常处理器);
}

yyvalue 获取当前异常处理器() {
    verify_yyvalue(当前异常处理器, true, 0); 
    return 当前异常处理器;
}
 
yyvalue 设置当前异常处理器(yyvalue 处理器) {
    当前异常处理器 = 处理器;
    return unit_to_yyvalue();
}
