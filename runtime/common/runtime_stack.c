

#include "common_include.h"

extern yy_ptr entryMain(yy_ptr*); // the entry to yy llvm ir

yy_ptr *stack;
yy_ptr *stack_end;
yy_ptr *stack_ptr;
// int64_t stack_size = 1024 * 128; // 1MB stack
int64_t stack_size = 1024 * 1024 * 1024 * 1 ; // 1GB stack
pthread_mutex_t stack_ptr_mutex = PTHREAD_MUTEX_INITIALIZER;


int64_t yy_increment_stack_ptr(int64_t increment) {
    // printf("stack_sz, cur %ld, totoal %ld\n", stack_ptr - stack, stack_size);
    stack_ptr += increment;
    if (stack_ptr >= stack_end) {
        errorAndAbort("Stack overflow");
    }
    else if (stack_ptr - stack >= stack_size / 2) {
        int64_t stack_ptr_offset =  stack_ptr - stack;
        while (stack_ptr - stack >= stack_size / 2) {
            stack_size *= 2;
        }
        stack = yy_gcReallocateBytes(stack, stack_size * sizeof(yy_ptr));
        stack_end = stack + stack_size;
        stack_ptr = stack + stack_ptr_offset;
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

int64_t yy_get_stack_offset(yy_ptr *ptr) {
    return ptr - stack;
}

// Define a function type for the pointer
typedef yy_ptr (*yy_function_type)(yy_ptr, yy_ptr, yy_ptr, yy_ptr);

yy_function_type ptr_to_function(yy_ptr ptr) {
    return (yy_function_type) ptr;
}

yy_ptr function_to_ptr (yy_function_type func) {
    return (yy_ptr) func;
}

int64_t prev_continuation_exception_id = 0;
int64_t *continuation_exception_table = NULL;
int64_t continuation_exception_table_size = 0;  
int64_t get_next_continuation_exception_id()
{
    int64_t next_continuation_exception_id = prev_continuation_exception_id + 1;
    if (continuation_exception_table_size <= next_continuation_exception_id) {
        if (continuation_exception_table_size == 0) {
            continuation_exception_table_size = 16;
        } else{
            continuation_exception_table_size = continuation_exception_table_size * 2;
        }
        continuation_exception_table = (int64_t*) realloc(continuation_exception_table, continuation_exception_table_size * sizeof(int64_t **));
    }
    prev_continuation_exception_id = next_continuation_exception_id;

    return next_continuation_exception_id;
}

/*
Continuation Exception Table Format:
just a stack offset
current stack offset (return address)
*/

int64_t *set_continuation_exception_handler(uint64_t *id, int64_t offset)
{
    continuation_exception_table[addr_to_int(id)] = offset;
    return 0;
}


int64_t yy_runtime_start() {
    const char* yy_debug_flag = getenv("YY_DEBUG_FLAG");
    // allocate a 100 M * 8 bytes stack
    
    // stack = (yy_ptr*) malloc(stack_size * sizeof(yy_ptr));
    stack = (yy_ptr*) yy_gcAllocateBytes(stack_size * sizeof(yy_ptr));
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
    yy_ptr return_record[5] = {(yy_ptr)(2), (yy_ptr) entryMain, NULL, int_to_addr(1), 0};
    yy_ptr initial_block_id = int_to_addr(1);

    /*
    Argument Format:
    [0]: current_stack_start, callee may use full stack space, when callee calls, it passes a stack offset indicating stack usage
    [1]: calling block id
    [2] : argument data
    [3]: return_record

    Callee may only modify the *contents of* return record and must not modify argument ptr

    */
    yy_ptr argument_record[4] = {(yy_ptr)stack_ptr, (yy_ptr)initial_block_id, NULL, (yy_ptr)return_record};
    yy_function_type current_function = NULL;


    // entryMain(&argument_record);
    while (true) {
        if (yy_debug_flag != NULL && strcmp(yy_debug_flag, "1") == 0) {
            printf("return_record is %p, %p, %p, %p, %p, stack, stack_ptr is %p, %p, current_function: %p\n", return_record[0], return_record[1], return_record[2], return_record[3], return_record[4], stack, stack_ptr, current_function);
        }
        switch (addr_to_int(return_record[0]))
        {
        case 1:
        {

            
            // get return value
            yy_ptr return_value = return_record[1];

            // get caller information from the stack
            yy_ptr stack_offset = stack_ptr[-3];
            yy_ptr caller_function = stack_ptr[-2];
            yy_ptr continuation_label_id = stack_ptr[-1];

            // restore stack
            pthread_mutex_lock(&stack_ptr_mutex);
            yy_decrement_stack_ptr(3);
            yy_decrement_stack_ptr(addr_to_int(stack_offset));
            pthread_mutex_unlock(&stack_ptr_mutex);

            // reset caller function
            current_function = ptr_to_function(caller_function);

            // we have reached the initial NULL function, and entryMain has returned
            if (current_function == NULL && stack_ptr == stack) {
                return 0;
            }

            // transfer control to caller
            return_record[0] = int_to_addr(4);
            return_record[1] = continuation_label_id;
            return_record[2] = return_value;
        }
            break;

        case 2: 
        {
            // get things from return record
            yy_function_type new_function = ptr_to_function(return_record[1]);
            yy_ptr argument_data = return_record[2];
            int64_t continuation_label_id = addr_to_int(return_record[3]);
            int64_t stack_offset = addr_to_int(return_record[4]);

            // save current function by saving 
            // 1. stack offset
            // 2. current function
            // 3. continuation label id
            
            pthread_mutex_lock(&stack_ptr_mutex);
            yy_increment_stack_ptr(stack_offset);
            stack_ptr[0] = int_to_addr(stack_offset);
            stack_ptr[1] = function_to_ptr(current_function);
            stack_ptr[2] = int_to_addr(continuation_label_id);
            yy_increment_stack_ptr(3);
            pthread_mutex_unlock(&stack_ptr_mutex);

            current_function = new_function;

            return_record[0] = int_to_addr(4);
            return_record[1] = initial_block_id;
            return_record[2] = argument_data;
        }
            break;

        case 3:
        {

            // get exception label
            yy_ptr exception_label = return_record[1];
            yy_ptr exception_data = return_record[2];

            // get exception handler
            int64_t offset = continuation_exception_table[addr_to_int(exception_label)];

            // restore stack
            pthread_mutex_lock(&stack_ptr_mutex);
            stack_ptr = stack + offset;
            pthread_mutex_unlock(&stack_ptr_mutex);

            // treat as a normal return, and continue
            return_record[0] = int_to_addr(1);
            return_record[1] = exception_data;
        }

            break;
        case 4:
        {
            yy_ptr transfer_block_id = return_record[1];
            yy_ptr argument_data = return_record[2];

            argument_record[0] = (yy_ptr)stack_ptr;
            argument_record[1] = transfer_block_id;
            argument_record[2] = argument_data;
            argument_record[3] = (yy_ptr)return_record;
            return_record[0] = int_to_addr(-1);
            current_function(argument_record[0], argument_record[1], argument_record[2], argument_record[3]);
        }
            break;
        
        default:
            errorAndAbort("Invalid return record");
            break;
        }

    }

    return -1;
}
