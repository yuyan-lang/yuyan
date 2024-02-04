

#include "common_include.h"
#include "memory_verifier.h"


yyvalue* stack_start;
yyvalue *stack_gc_limit;
yyvalue* stack_end;
yyvalue* stack_ptr;
// uint64_t stack_size = 1024 * 1024 * 32; // 32M array size, 256MB stack
uint64_t stack_size = 1024 * 1024 * 128 ; // 2GB stack
double stack_gc_limit_percentage = 80.0;
pthread_mutex_t stack_ptr_mutex = PTHREAD_MUTEX_INITIALIZER;
yy_function_type current_function;
bool yy_debug_flag = false;

yyvalue yy_pre_function_call_info(yyvalue new_stack_ptr_val, yyvalue next_fun_ptr, yyvalue stack_occupation_arg){

    yy_function_type next_fun = yyvalue_to_funcptr(next_fun_ptr);
    uint64_t stack_occupation = yyvalue_to_int(stack_occupation_arg);
    yyvalue *new_stack_ptr = yyvalue_to_stackptr(new_stack_ptr_val);
    if (stack_ptr < new_stack_ptr) {
        assert(yyvalue_to_stackptr(new_stack_ptr[0]) == stack_ptr);
    } else {
        assert(stack_ptr == new_stack_ptr);
    }
    assert(stack_start <= stack_ptr && stack_ptr <= stack_gc_limit);
    assert(stack_start <= new_stack_ptr && new_stack_ptr <= stack_gc_limit);
    assert(current_heap <= current_allocation_ptr && current_allocation_ptr <= current_heap_gc_limit);
    verify_current_heap();
    verify_stack();

    if (yy_debug_flag)
    {
        fprintf(stderr,
                "Calling function with RET stack ptr "
        );
        yy_print_yyvalue(new_stack_ptr[0], 0);
        fprintf(stderr,
                " (offset %td)", yyvalue_to_stackptr(new_stack_ptr[0]) - stack_start);
        fprintf(stderr,
                "\n CURRENT stack ptr %p (offset %td) NEXT stack ptr %p (offset %td)",
                stack_ptr, stack_ptr - stack_start, new_stack_ptr, new_stack_ptr - stack_start
        );
        fprintf(stderr,
                "\n RET func ptr ");
        yy_print_yyvalue(new_stack_ptr[1], 0);
        fprintf(stderr,
                "\n RET continuation id "
        );
        yy_print_yyvalue(new_stack_ptr[2], 0);
        fprintf(stderr,
                "\n RET arg "
        );
        yy_print_yyvalue(new_stack_ptr[3], 0);
        fprintf(stderr,
                "\n calling block id "
        );
        yy_print_yyvalue(new_stack_ptr[4], 0);
        fprintf(stderr,
                "\n calling function "
        );
        yy_print_yyvalue(next_fun_ptr, 0);
        if (stack_occupation <= 5)
        {
            fprintf(stderr,
                    "\n (no args) "
            );
        } else {
            for (int i = 5; i < stack_occupation; i++) {
                fprintf(stderr,
                        "\n arg %d ", i - 5
                );
                yy_print_yyvalue(new_stack_ptr[i], 0);
            }
        }
        fprintf(stderr,
                "\n");
    }

    
    return unit_to_yyvalue();
}
// runtime invokes this function prior to any function call
// the primary task of this function is to ensure stack does not overflow, and invoke gc if necessary
yyvalue yy_pre_function_call_gc(yyvalue new_stack_ptr, yyvalue stack_occupy_arg){
    uint64_t stack_occupy = yyvalue_to_int(stack_occupy_arg);
    assert (stack_occupy >= 5);

    assert(stack_ptr <= yyvalue_to_stackptr(new_stack_ptr));
    stack_ptr = yyvalue_to_stackptr(new_stack_ptr);
    assert(current_allocation_ptr > current_heap_gc_limit);
    if (current_allocation_ptr > current_heap_end)
    {
        fprintf(stderr, "[RS] No space left and garbage collection cannot be performed yet. \n"
                        "Heap Start %p, Heap End %p, Heap GC Limit %p, Allocation Pointer %p \n"
                        "Heap Size %td, Heap Offset %td, GC Point Size %td, \n",
                current_heap, current_heap_end, current_heap_gc_limit, current_allocation_ptr,
                current_heap_end - current_heap,
                current_allocation_ptr - current_heap,
                current_heap_gc_limit - current_heap);
        errorAndAbort("No space left in the major heap, make GC occur earlier by adjusting the GC limit");
        return unit_to_yyvalue();
    }
    if (stack_ptr >= stack_gc_limit) {
        errorAndAbort("Stack overflowed, please increase stack size and try again");
        return unit_to_yyvalue();
    }
    stack_ptr += stack_occupy;
    yy_perform_gc();
    stack_ptr -= stack_occupy;
    return unit_to_yyvalue();
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

#define STACK_MEMORY_LIMIT 100000
yyvalue stack_memory[STACK_MEMORY_LIMIT];
// uint64_t stack_memory_size[100000];

yyvalue set_continuation_exception_handler(yyvalue id, yyvalue stack_ptr_address)
{
    assert(yyvalue_to_stackptr(stack_ptr_address) == stack_ptr); // caling function can only return to its return address
    continuation_exception_table[yyvalue_to_int(id)] = stack_ptr_address;
    // stack_memory_size[yyvalue_to_int(id)] = yyvalue_to_stackptr(stack_ptr_address) - stack_start;
    if (yyvalue_to_int(id) < STACK_MEMORY_LIMIT) {
        stack_memory[yyvalue_to_int(id)] = yyvalue_to_stackptr(stack_ptr_address)[1]; // this is the return address (we make sure the return address are the same)
    }
    // memcpy(stack_memory[yyvalue_to_int(id)], stack_start, (yyvalue_to_stackptr(stack_ptr_address) - stack_start) * sizeof(yyvalue));
    return unit_to_yyvalue();
}

yyvalue get_continuation_exception_handler(yyvalue id)
{
    yyvalue ret = continuation_exception_table[yyvalue_to_int(id)];
    uint64_t id_num = yyvalue_to_int(id);
    // fprintf(stderr, "Getting exception handler %lu\n", yyvalue_to_int(id));
    assert(yyvalue_to_stackptr(ret) <= stack_ptr); // this is an informal check that the calling function should not return to a point that is after the current stack pointer
    if (yyvalue_to_int(id) < STACK_MEMORY_LIMIT) {
        assert(stack_memory[yyvalue_to_int(id)] == yyvalue_to_stackptr(ret)[1]);
    }
    // assert(stack_memory_size[yyvalue_to_int(id)] == yyvalue_to_stackptr(ret) - stack_start);
    // assert(memcmp(stack_memory[yyvalue_to_int(id)], stack_start, (yyvalue_to_stackptr(ret) - stack_start) * sizeof(yyvalue)) == 0);
    // this is nonexhaustive check
    return ret;
}


// yyvalue yy_set_stack_ptr(yyvalue new_stack_ptr_address)
// {
//     yyvalue* new_stack_ptr = yyvalue_to_stackptr(new_stack_ptr_address);
//     // const char* yy_debug_flag = getenv("YY_DEBUG_FLAG");
//     // if (yy_debug_flag != NULL && strcmp(yy_debug_flag, "1") == 0) {
//     //     fprintf(stderr, "Setting stack pointer, new_offset = %" PRIu64 " prev_offset = %" PRIu64 "\n", 
//     //         new_stack_ptr - stack_start, stack_ptr - stack_start
//     //     );
//     // }
//     stack_ptr = new_stack_ptr;
//     return unit_to_yyvalue();
// }


void yy_exit_function(yyvalue stack_top, yyvalue current_allocation_arg){
    yyvalue *local_stack_ptr = yyvalue_to_stackptr(stack_top);
    stack_ptr = local_stack_ptr;
    assert(stack_ptr == stack_start);
    assert(yyvalue_to_int(stack_ptr[4]) == 1);
    return;
}

#define RECURSIVE_VERIFICATION_WHEN_RUNNING true
int64_t yy_runtime_start() {
    yy_debug_flag = getenv("YY_DEBUG_FLAG") != NULL && strcmp(getenv("YY_DEBUG_FLAG"), "1") == 0;
    // allocate a 100 M * 8 bytes stack

    // stack = (yyvalue*) malloc(stack_size * sizeof(yyvalue));
    stack_start = (yyvalue*) malloc(stack_size * sizeof(yyvalue));
    stack_end = stack_start + stack_size;
    // TODO: we should allow stack growth
    stack_gc_limit = stack_start + (uint64_t) (stack_size * stack_gc_limit_percentage / 100.0);
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

    entryMain(stackptr_to_yyvalue(stack_start), runtime_heap_pointer_to_yyvalue(current_allocation_ptr));

    return 0;
}
