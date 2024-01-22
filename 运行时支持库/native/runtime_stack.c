

#include "common_include.h"
#include "memory_verifier.h"


yyvalue *stack;
yyvalue *stack_end;
yyvalue *stack_ptr;
uint64_t stack_size = 1024 * 1024 * 32; // 32M array size, 256MB stack
// int64_t stack_size = 1024 * 1024 * 1024 * 1 ; // 1GB stack
pthread_mutex_t stack_ptr_mutex = PTHREAD_MUTEX_INITIALIZER;
yy_function_type current_function;

int64_t yy_increment_stack_ptr(int64_t increment) {
    // printf("stack_sz, cur %ld, totoal %ld\n", stack_ptr - stack, stack_size);
    stack_ptr += increment;
    if (stack_ptr >= stack_end) {
        errorAndAbort("Stack overflow");
    }
    else if (stack_ptr - stack >= stack_size / 2) {
        // uncomment to add stack reallocation
        // I am commenting them to prepare for stack allocation
        // int64_t stack_ptr_offset =  stack_ptr - stack;
        // while (stack_ptr - stack >= stack_size / 2) {
        //     stack_size *= 2;
        // }
        // if (use_libgc){
        //     stack = yy_gcReallocateBytes(stack, stack_size * sizeof(yyvalue));
        // } else {
        //     stack = (yyvalue*) realloc(stack, stack_size * sizeof(yyvalue));
        // }
        // stack_end = stack + stack_size;
        // stack_ptr = stack + stack_ptr_offset;
    }
    return 0;
}

int64_t yy_decrement_stack_ptr(int64_t increment) {
    stack_ptr -= increment;
    if (stack_ptr < stack) {
        errorAndAbort("Stack underflow");
    }
    return 0;
}

yyvalue yy_get_stack_offset(yyvalue ptr) {
    return int_to_yyvalue(yyvalue_to_stackptr(ptr) - stack);
}


uint64_t prev_continuation_exception_id = 0;
uint64_t *continuation_exception_table = NULL;
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
        continuation_exception_table = (uint64_t*) realloc(continuation_exception_table, continuation_exception_table_size * sizeof(uint64_t **));
    }
    prev_continuation_exception_id = next_continuation_exception_id;

    return int_to_yyvalue(next_continuation_exception_id);
}

/*
Continuation Exception Table Format:
just a stack offset
current stack offset (return address)
*/

yyvalue set_continuation_exception_handler(yyvalue id, yyvalue offset)
{
    continuation_exception_table[yyvalue_to_int(id)] = yyvalue_to_int(offset);
    return unit_to_yyvalue();
}

void begin_stack_manipulation(){
    if (use_profiler){
        pthread_mutex_lock(&stack_ptr_mutex);
    }
}

void end_stack_manipulation(){
    if (use_profiler){
        pthread_mutex_unlock(&stack_ptr_mutex);
    }
}


int64_t yy_runtime_start() {
    const char* yy_debug_flag = getenv("YY_DEBUG_FLAG");
    // allocate a 100 M * 8 bytes stack
    
    // stack = (yyvalue*) malloc(stack_size * sizeof(yyvalue));
    if (use_libgc) {
        stack = (yyvalue*) GC_MALLOC(stack_size * sizeof(yyvalue));
    } else {
        stack = (yyvalue*) malloc(stack_size * sizeof(yyvalue));
    }
    stack_end = stack + stack_size;
    stack_ptr = stack;

    /*
    Record Format:
    [0]:
        1 : return to the caller with [1] = return value
        2 : call function, with [1] = function pointer, [2] = argument data, [3] = continuation label id, [4] stack offset
        3 : continuation exception return to caller with [1] = exception label [2] exception data
        4 : local control transfer, with [1] = continuation label id [2] = argument data

    */
    yyvalue return_record_raw[5] = {int_to_yyvalue(2), funcptr_to_yyvalue(entryMain), unit_to_yyvalue(), int_to_yyvalue(1), int_to_yyvalue(0)};
    yyvalue return_record = raw_tuple_to_yyvalue(5, return_record_raw);
    yyvalue initial_block_id = int_to_yyvalue(1);

    /*
    Argument Format:
    [0]: current_stack_start, callee may use full stack space, when callee calls, it passes a stack offset indicating stack usage
    [1]: calling block id
    [2] : argument data
    [3]: return_record

    Callee may only modify the *contents of* return record and must not modify argument ptr

    */
    // yyvalue argument_record[4] = {(yyvalue)stack_ptr, (yyvalue)initial_block_id, NULL, (yyvalue)return_record};
    current_function =(yy_function_type) NULL;
    if (use_profiler) {
        start_yy_profiler();
    }

    // entryMain(&argument_record);
    while (true) {
        if (yy_debug_flag != NULL && strcmp(yy_debug_flag, "1") == 0) {
            printf("return_record is %p, %p, %p, %p, %p, stack, stack_ptr is %p, %p, current_function: %p\n", 
                yyvalue_to_generic_ptr(yy_read_tuple(return_record, 0)), 
                yyvalue_to_generic_ptr(yy_read_tuple(return_record, 1)), 
                yyvalue_to_generic_ptr(yy_read_tuple(return_record, 2)), 
                yyvalue_to_generic_ptr(yy_read_tuple(return_record, 3)), 
                yyvalue_to_generic_ptr(yy_read_tuple(return_record, 4)), 
                stack, 
                stack_ptr,
                current_function);
        }
        switch (yyvalue_to_int(yy_read_tuple(return_record, 0)))
        {
        case 1:
        {

            
            // get return value
            yyvalue return_value = yy_read_tuple(return_record, 1);

            // get caller information from the stack
            yyvalue stack_offset = stack_ptr[-3];
            yyvalue caller_function = stack_ptr[-2];
            yyvalue continuation_label_id = stack_ptr[-1];


            // restore stack
            begin_stack_manipulation();
            yy_decrement_stack_ptr(3);
            yy_decrement_stack_ptr(yyvalue_to_int(stack_offset));
            end_stack_manipulation();

            // reset caller function
            current_function = yyvalue_to_funcptr(caller_function);

            // we have reached the initial NULL function, and entryMain has returned
            if (current_function == NULL && stack_ptr == stack) {
                return 0;
            }


            // transfer control to caller
            yy_write_tuple(return_record, 0, int_to_yyvalue(4));
            yy_write_tuple(return_record, 1, continuation_label_id);
            yy_write_tuple(return_record, 2, return_value);
        }
            break;

        case 2: 
        {
            // get things from return record
            yyvalue new_func_value = yy_read_tuple(return_record, 1);
            yy_function_type new_function = yyvalue_to_funcptr(new_func_value);
            yyvalue argument_data = yy_read_tuple(return_record, 2);
            yyvalue continuation_label_id = (yy_read_tuple(return_record, 3));
            int64_t stack_offset = yyvalue_to_int(yy_read_tuple(return_record, 4));

            // save current function by saving 
            // 1. stack offset
            // 2. current function
            // 3. continuation label id
            
            // pthread_mutex_lock(&stack_ptr_mutex);
            begin_stack_manipulation();
            yy_increment_stack_ptr(stack_offset);
            stack_ptr[0] = int_to_yyvalue(stack_offset);
            stack_ptr[1] = funcptr_to_yyvalue(current_function);
            stack_ptr[2] = (continuation_label_id);
            yy_increment_stack_ptr(3);
            end_stack_manipulation();
            // pthread_mutex_unlock(&stack_ptr_mutex);

            // perform gc
            if (tiny_heap_offset != 0) {
                verify_yyvalue(argument_data, true, 0);
                yy_perform_gc(&argument_data);
            }


            current_function = new_function;

            yy_write_tuple(return_record, 0, int_to_yyvalue(4));
            yy_write_tuple(return_record, 1, initial_block_id);
            yy_write_tuple(return_record, 2, argument_data);
        }
            break;

        case 3:
        {

            // get exception label
            yyvalue exception_label = yy_read_tuple(return_record, 1);
            yyvalue exception_data = yy_read_tuple(return_record, 2);

            // get exception handler
            int64_t offset = continuation_exception_table[yyvalue_to_int(exception_label)];

            // restore stack
            // pthread_mutex_lock(&stack_ptr_mutex);
            begin_stack_manipulation();
            stack_ptr = stack + offset;
            end_stack_manipulation();
            // pthread_mutex_unlock(&stack_ptr_mutex);

            // treat as a normal return, and continue
            yy_write_tuple(return_record, 0, int_to_yyvalue(1));
            yy_write_tuple(return_record, 1, exception_data);
        }

            break;
        case 4:
        {
            yyvalue transfer_block_id = yy_read_tuple(return_record, 1);
            yyvalue argument_data = yy_read_tuple(return_record, 2);

            yy_write_tuple(return_record, 0, int_to_yyvalue(-1));
            current_function(
                stackptr_to_yyvalue(stack_ptr),
                transfer_block_id,
                argument_data,
                return_record);

        }
            break;
        
        default:
            errorAndAbort("Invalid return record");
            break;
        }

    }

    return -1;
}
