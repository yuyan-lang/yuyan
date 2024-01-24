#include "../common_include.h"


typedef struct {
    int exit_status;
} my_data_t;

void child_exit_cb(uv_process_t* process, int64_t exit_status, int term_signal) {
    my_data_t* data = (my_data_t*)process->data;
    if (term_signal != 0){
        data->exit_status = term_signal + 128;
    }
    else
    {
        data->exit_status = exit_status;
    }
    uv_close((uv_handle_t*)process, NULL);
}




// https://stackoverflow.com/questions/14751504/capture-a-child-processs-stdout-with-libuv
// program is a string var, arguments is a list of string vars!
yyvalue yyRunProcessGetOutputSync(yyvalue program, yyvalue arguments)
{
    char* programName = yyvalue_to_string(program);
    uint64_t argumentCount = iso_list_get_length(arguments);
    yyvalue* argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = yyvalue_to_string(argumentArray[i]);
    }
    args[argumentCount+1]= NULL;


    uv_pipe_t stdOutPipe = {};
    uv_pipe_init(uv_global_loop, &stdOutPipe, 0);
    // uv_pipe_open(&stdOutPipe, 0);

    uv_pipe_t stdErrPipe = {};
    uv_pipe_init(uv_global_loop, &stdErrPipe, 0);
    // uv_pipe_open(&stdErrPipe, 0);

    uv_process_options_t options = {0};

    uv_stdio_container_t child_stdio[3] = {};
    child_stdio[0].flags = UV_IGNORE;
    child_stdio[1].flags = UV_CREATE_PIPE |  UV_WRITABLE_PIPE;
    child_stdio[1].data.stream = (uv_stream_t *) &stdOutPipe;
    child_stdio[2].flags = UV_CREATE_PIPE |  UV_WRITABLE_PIPE;
    child_stdio[2].data.stream = (uv_stream_t *) &stdErrPipe;
    // child_stdio[1].flags = UV_IGNORE;

    options.stdio_count = 3;
    options.stdio = child_stdio;
    options.file = args[0];
    options.args = args;

    options.exit_cb = child_exit_cb;



    uv_process_t child_req;
    my_data_t my_data;
    child_req.data = &my_data;

    int r;
    if ((r = uv_spawn(uv_global_loop, &child_req, &options))) {
        fprintf(stderr, "uv_spawn error: (custom) %s:  %s\n", programName,  uv_strerror(r));
        errorAndAbort("spawn error");
        // return 0;
    }

    // uv_read_start((uv_stream_t *)&stdOutPipe, alloc_buffer, on_read);
    // uv_read_start((uv_stream_t *)&stdErrPipe, alloc_buffer, on_read);
    readStreamUntilEofIntoDataAync((uv_stream_t *)&stdOutPipe);
    readStreamUntilEofIntoDataAync((uv_stream_t *)&stdErrPipe);


    uv_run(uv_global_loop, UV_RUN_DEFAULT);

   

    // char* stdOutOutput = (char * )stdOutPipe.data;
    // char* stdErrOutput = (char * )stdErrPipe.data;

    uint64_t stdOutBufLength = strlen((char *)stdOutPipe.data) + 1;
    uint64_t stdErrBufLength = strlen((char *)stdErrPipe.data) + 1;
    char *stdOutOutput = malloc(stdOutBufLength);
    char *stdErrOutput = malloc(stdErrBufLength);

    strcpy(stdOutOutput, (char*)stdOutPipe.data);
    strcpy(stdErrOutput, (char*)stdErrPipe.data);

    free(stdOutPipe.data);
    free(stdErrPipe.data);

    int64_t child_exit_status = my_data.exit_status;

    yyvalue stdOutVal = malloc_string_to_yyvalue(stdOutBufLength, stdOutOutput);
    yyvalue stdErrVal = malloc_string_to_yyvalue(stdErrBufLength, stdErrOutput);
    free(stdOutOutput);
    free(stdErrOutput);

    yyvalue results[] = {
        bool_to_yyvalue(child_exit_status==0),
        stdOutVal,
        stdErrVal
    };

    // print debug
    // {
    //     fprintf(stderr, "%s : ", programName);
    //     for(int i = 0; i < argumentCount; i ++){
    //         fprintf(stderr, "%s , ", args[i+1]);
    //     }
    //     fprintf(stderr, " : %s", stdOutOutput);
    // }

    yyvalue resultTuple = tuple_to_yyvalue(3, results);

    return resultTuple;

    // return unit_to_yyvalue();
}


// void on_exit(uv_process_t *req, int64_t exit_status, int term_signal) {
//     fprintf(stderr, "Process exited with status %d, signal %d\n", exit_status, term_signal);
//     uv_close((uv_handle_t*) req, NULL);
// }

yyvalue yyRunProcessSync(yyvalue program, yyvalue arguments)
{
    char* programName = yyvalue_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    yyvalue* argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = yyvalue_to_string((argumentArray[i]));
    }
    args[argumentCount+1]= NULL;


    uv_process_options_t options = {};

    options.file = args[0];
    options.args = args;
    // options.flags = 0;
    // options.exit_cb = on_exit;

    options.exit_cb = child_exit_cb;

    uv_process_t child_req;
    my_data_t my_data;
    child_req.data = &my_data;
    
    int r;
    if ((r = uv_spawn(uv_global_loop, &child_req, &options))) {
        fprintf(stderr, "uv_spawn [yyrunnooutput] error: (custom) %s\n", uv_strerror(r));
        // return 0;
    }

    int64_t child_exit_status = my_data.exit_status;

    uv_run(uv_global_loop, UV_RUN_DEFAULT);

    return bool_to_yyvalue(child_exit_status == 0);
}

yyvalue yyRunProcessSyncPipeOutput(yyvalue program, yyvalue arguments)
{
    char* programName = yyvalue_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    yyvalue* argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = yyvalue_to_string((argumentArray[i]));
    }
    args[argumentCount+1]= NULL;


    uv_process_options_t options = {};

    uv_stdio_container_t child_stdio[3] = {};
    child_stdio[0].flags = UV_IGNORE;
    child_stdio[1].flags = UV_INHERIT_FD;
    child_stdio[1].data.fd = 1;
    child_stdio[2].flags = UV_INHERIT_FD;
    child_stdio[2].data.fd = 2;
    // child_stdio[1].flags = UV_IGNORE;

    options.stdio_count = 3;
    options.stdio = child_stdio;
    options.file = args[0];
    options.args = args;
    // options.flags = 0;
    // options.exit_cb = on_exit;
    options.exit_cb = child_exit_cb;


    uv_process_t child_req;
    my_data_t my_data;
    child_req.data = &my_data;
    
    int r;
    if ((r = uv_spawn(uv_global_loop, &child_req, &options))) {
        fprintf(stderr, "uv_spawn [yyrunnooutput] error: (custom) %s\n", uv_strerror(r));
        // return 0;
    }


    uv_run(uv_global_loop, UV_RUN_DEFAULT);

    int64_t child_exit_status = my_data.exit_status;
    
    return int_to_yyvalue(child_exit_status);
    // return bool_to_yyvalue(child_exit_status == 0);
}

