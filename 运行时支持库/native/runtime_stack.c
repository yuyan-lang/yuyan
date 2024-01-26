

#include "common_include.h"
#include "memory_verifier.h"


yyvalue* stack_start;
yyvalue* stack_end;
yyvalue* stack_ptr;
// uint64_t stack_size = 1024 * 1024 * 32; // 32M array size, 256MB stack
uint64_t stack_size = 1024 * 1024 * 128 ; // 2GB stack
pthread_mutex_t stack_ptr_mutex = PTHREAD_MUTEX_INITIALIZER;
yy_function_type current_function;

// runtime invokes this function prior to any function call
// the primary task of this function is to ensure stack does not overflow, set the global stack pointer, and invoke gc if necessary
yyvalue pre_function_call(){
    if (stack_ptr + 3 >= stack_end) {
        errorAndAbort("Stack overflowed, please increase stack size and try again");
        return unit_to_yyvalue();
    } else {
        if (current_allocation_ptr > current_heap_gc_limit) {
            stack_ptr += 5;
            yy_perform_gc();
            stack_ptr -= 5;
        }
        return unit_to_yyvalue();
    }
}


uint64_t prev_continuation_exception_id = 0;
yyvalue* continuation_exception_table = NULL;
uint64_t continuation_exception_table_size = 0;  
yyvalue get_next_continuation_exception_id()
{
    uint64_t next_continuation_exception_id = prev_continuation_exception_id + 1;
    if (continuation_exception_table_size <= next_continuation_exception_id) {
        if (continuation_exception_table_size == 0) {
            continuation_exception_table_size = 16;
        } else{
            continuation_exception_table_size = continuation_exception_table_size * 2;
        }
        continuation_exception_table = (yyvalue*) realloc(continuation_exception_table, continuation_exception_table_size * sizeof(yyvalue));
    }
    prev_continuation_exception_id = next_continuation_exception_id;

    return int_to_yyvalue(next_continuation_exception_id);
}

/*
Continuation Exception Table Format:
stores the stack pointer, supposedly, raise an exception is the same thing as returning to a particular call point
*/

yyvalue set_continuation_exception_handler(yyvalue id, yyvalue stack_ptr_address)
{
    continuation_exception_table[yyvalue_to_int(id)] = stack_ptr_address;
    return unit_to_yyvalue();
}

yyvalue get_continuation_exception_handler(yyvalue id)
{
    return continuation_exception_table[yyvalue_to_int(id)];
}

yyvalue yy_set_stack_ptr(yyvalue new_stack_ptr_address)
{
    stack_ptr = yyvalue_to_stackptr(new_stack_ptr_address);
    return unit_to_yyvalue();
}


void yy_exit_function(){
    assert(stack_ptr == stack_start);
    assert(stack_ptr[4] == 1);
    return;
}

#define RECURSIVE_VERIFICATION_WHEN_RUNNING true
int64_t yy_runtime_start() {
    const char* yy_debug_flag = getenv("YY_DEBUG_FLAG");
    // allocate a 100 M * 8 bytes stack
    
    // stack = (yyvalue*) malloc(stack_size * sizeof(yyvalue));
    stack_start = (yyvalue*) malloc(stack_size * sizeof(yyvalue));
    stack_end = stack_start + stack_size;
    stack_ptr = stack_start;

    if (use_profiler) {
        // start_yy_profiler();
    }

    /**
     * Stack will be arranged like this
     * 
     * Yuyan Call Routine
     * 
     * For function call, the following values will be pushed to the stack
     * 1. Previous stack_ptr
     * 2. Previous function pointer
     * 3. Continuation id label
     * 
     * 
     * In callee, the argument will be stored in stack_ptr[0], the calling block id will be directly passed as an argument
     * When, returning, the return value will be stored in stack_ptr[0]
     * 
     * 
     * 
     * 
    */
    stack_start[0] = stackptr_to_yyvalue(stack_start);
    stack_start[1] = funcptr_to_yyvalue(yy_exit_function);
    stack_start[2] = int_to_yyvalue(1);
    stack_start[3] = unit_to_yyvalue(); // entryMain 's argument value
    stack_start[4] = int_to_yyvalue(1);  // entryMain 's calling block id


    entryMain();

    return -1;
}
