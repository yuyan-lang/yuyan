

#include "common_include.h"

extern int64_t* entryMain(int64_t**); // the entry to yy llvm ir

int64_t **stack;
int64_t **stack_end;
int64_t **stack_ptr;
int64_t stack_size = 100000000;

int64_t yy_increment_stack_ptr(int64_t increment) {
    stack_ptr += increment;
    if (stack_ptr >= stack_end) {
        errorAndAbort("Stack overflow");
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

// Define a function type for the pointer
typedef int64_t* (*yy_function_type)(int64_t **);

yy_function_type ptr_to_function(int64_t *ptr) {
    return (yy_function_type) ptr;
}

int64_t *function_to_ptr (yy_function_type func) {
    return (int64_t *) func;
}

int64_t prev_continuation_exception_id = 0;
int64_t ***continuation_exception_table = NULL;
int64_t continuation_exception_table_size = 0;  
int64_t *get_next_continuation_exception_id()
{
    int64_t next_continuation_exception_id = prev_continuation_exception_id + 1;
    if (continuation_exception_table_size <= next_continuation_exception_id) {
        if (continuation_exception_table_size == 0) {
            continuation_exception_table_size = 16;
        } else{
            continuation_exception_table_size = continuation_exception_table_size * 2;
        }
        continuation_exception_table = (int64_t ***) realloc(continuation_exception_table, continuation_exception_table_size * sizeof(int64_t **));
    }

    return int_to_addr(next_continuation_exception_id);
}

/*
Continuation Exception Table Format:
[0]: function pointer
[1]: function label id
[2]: current stack pointer
*/

int64_t *set_continuation_exception_handler(int64_t *id, int64_t **handler)
{
    continuation_exception_table[addr_to_int(id)] = handler;
    return id;
}

int64_t yy_runtime_start() {
    // allocate a 100 M * 8 bytes stack
    
    stack = (int64_t **) malloc(stack_size * sizeof(int64_t));
    stack_end = stack + stack_size;
    stack_ptr = stack;

    /*
    Record Format:
    [0]:
        1 : return to the caller with [1] = return value
        2 : call function, with [1] = function pointer, [2] = argument data, [3] = continuation label id, [4] stack offset
        3 : continuation exception return to caller with [1] = exception label [2] exception data

    */
    int64_t *return_record[5];
    int64_t *initial_block_id = int_to_addr(1);

    /*
    Argument Format:
    [0]: current_stack_start, callee may use full stack space, when callee calls, it passes a stack offset indicating stack usage
        the argument will be passed as the first element in the stack
    [1]: calling block id
    [3]: return_record

    Callee may only modify the *contents of* return record and must not modify argument ptr

    */
    int64_t *argument_record[4] = {stack_ptr, initial_block_id, NULL, return_record};
    yy_function_type current_function = entryMain;

    entryMain(&argument_record);
    while (true) {
        switch (addr_to_int(return_record[0]))
        {
        case 1:
            if (current_function == entryMain) {
                errorAndAbort("TODO");
                // we're done
                return 0;
            }
            // get return value
            int64_t *return_value = return_record[1];

            // get caller information from the stack
            int64_t *stack_offset = stack_ptr[-3];
            int64_t *caller_function = stack_ptr[-2];
            int64_t *continuation_label_id = stack_ptr[-1];

            // restore stack
            yy_decrement_stack_ptr(3);
            yy_decrement_stack_ptr(stack_offset);

            // prepare arguments
            argument_record[0] = stack_ptr;
            argument_record[1] = continuation_label_id;
            argument_record[2] = return_value;
            argument_record[3] = return_record;

            // call caller function
            current_function = ptr_to_function(caller_function);
            current_function(argument_record);
            break;

        case 2: /* call yuyan function */
            // get things from return record
            yy_function_type new_function = ptr_to_function(return_record[1]);
            int64_t *argument_data = return_record[2];
            int64_t continuation_label_id = addr_to_int(return_record[3]);
            int64_t stack_offset = addr_to_int(return_record[4]);

            // save current function by saving 
            // 1. stack offset
            // 2. current function
            // 3. continuation label id
            yy_increment_stack_ptr(stack_offset);
            stack_ptr[0] = stack_offset;
            stack_ptr[1] = function_to_ptr(current_function);
            stack_ptr[2] = int_to_addr(continuation_label_id);
            yy_increment_stack_ptr(3);

            argument_record[0] = stack_ptr;
            argument_record[1] = initial_block_id;
            argument_record[2] = argument_data;
            argument_record[3] = return_record;
            current_function = new_function;
            current_function(argument_record);
            break;

        case 3:

            // get exception label
            int64_t *exception_label = return_record[1];
            int64_t *exception_data = return_record[2];

            // get exception handler
            int64_t **exception_handler = continuation_exception_table[addr_to_int(exception_label)];

            // get caller information from the stack
            int64_t *caller_function = exception_handler[0];
            int64_t *caller_function_label = exception_handler[1];

            // restore stack
            stack_ptr = exception_handler[2];

            // prepare arguments
            argument_record[0] = stack_ptr;
            argument_record[1] = caller_function_label;
            argument_record[2] = exception_data;
            argument_record[3] = return_record;

            // call caller function
            current_function = ptr_to_function(caller_function);
            current_function(argument_record);
            break;
        
        default:
            errorAndAbort("Invalid return record");
            break;
        }

    }

    return -1;
}
