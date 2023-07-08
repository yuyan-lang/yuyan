

#include "common_include.h"

int matchException(char* index){
    printf("ERROR: A match exception has occurred. \n");
    printf("index is %s\n", index);
    exit(1);
    return -1;
}

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

uint64_t yyThrowException(yy_ptr err){
    
    fprintf(stderr, "%s", addr_to_string(err));
    exit(1);
    return -1;
}

// I believe abssingle has two arguments , the first is just the closure itself
uint64_t yyUncaughtException(yy_ptr closure, yy_ptr dynclsfdVal){
    // yy_ptr t = data_to_addr(dynclsfdVal[2]);
    // fprintf(stderr, "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n豫言运行环境(yy_runtime)：未捕捉的异常(Uncaught Exception)：%s\n", addr_to_string(t));
    // updated version that removes the use of dynamically classified value in exceptions
    // we do not use dynclsfd but use string directly. Apparently v0.1 has problems with dynclsfd value
    fprintf(stderr, "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n豫言运行环境(yy_runtime)：未捕捉的异常(Uncaught Exception)：\n");
    fprintf(stderr, "尝试打印值：（可能会出现segfault）：\n");
    // fprintf(stderr, "%s", (char*)(data_to_addr(dynclsfdVal[3])));
    fprintf(stderr, "%s", (addr_to_string(dynclsfdVal)));
    fprintf(stderr, "\n");
    exit(1);
    return -1;
}

yy_ptr currentExceptionHandler;

void initialize_global_exception_handler(){
    currentExceptionHandler =  function_to_addr(&yyUncaughtException);
}


yy_ptr yyGetCurrentExceptionHandler() {
    return currentExceptionHandler;
}
 
yy_ptr yySetCurrentExceptionHandler(yy_ptr toSet) {
    currentExceptionHandler = toSet;
    return unit_to_addr();
}

// I believe abssingle has two arguments , the first is just the closure itself
uint64_t 全局异常处理器(int64_t* closure, char* errorMsg, int64_t* kont){
    fprintf(stderr, "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n豫言运行环境(yy_runtime)：未捕捉的异常(Uncaught Exception)：\n");
    fprintf(stderr, "尝试打印值：（可能会出现 异常）：\n");
    fflush(stderr);
    fprintf(stderr, "%s", errorMsg);
    fprintf(stderr, "\n");
    exit(1);
    return -1;
}

// // I believe abssingle has two arguments , the first is just the closure itself
// uint64_t 未赋值递归函数(int64_t* closure, char* kont){
//     fprintf(stderr, "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n豫言运行环境(yy_runtime)：不支持的递归表达式，你的递归表达式必须是函数，递归表达式在完成运算前不可以调用自身。\n");
//     fprintf(stderr, "\n");
//     exit(1);
//     return -1;
// }



void* 当前异常处理器;
extern void* 内部全局异常处理器(void* arg1, void* arg2, void* arg3);


void yy_豫言初始化全局异常处理器(){
    void **tup = (void**) yy_gcAllocateArray(2);
    tup[0] = 内部全局异常处理器;
    当前异常处理器 = tup;
}

void* 获取当前异常处理器() {
    return 当前异常处理器;
}
 
void* 设置当前异常处理器(void* 处理器) {
    当前异常处理器 = 处理器;
    return 0;
}
