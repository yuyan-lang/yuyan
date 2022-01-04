#include "../globalInclude.h"



void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
  buf->base = GC_MALLOC(suggested_size);
  buf->len = suggested_size;
}

//https://github.com/libuv/help/issues/22
void on_read(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
     if (nread < 0) {
         if (nread != UV_EOF)
             fprintf(stderr, "Read error %s\n", uv_err_name(nread));
         uv_close((uv_handle_t*) client, NULL);
         return;
     }
 
     char* data = (char*) GC_MALLOC(sizeof(char) * (nread+1));
     data[nread] = '\0';
     strncpy(data, buf->base, nread);
 
    client->data = data;
     fprintf(stderr, "%s", data);
 }


// https://stackoverflow.com/questions/14751504/capture-a-child-processs-stdout-with-libuv
// program is a string var, arguments is a list of string vars!
uint64_t* yyRunProcessGetOutputSync(uint64_t* program, uint64_t* arguments)
{
    char* programName = addr_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    uint64_t** argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = string_to_addr(argumentArray[i]);
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




    uv_process_t child_req;

    int r;
    if ((r = uv_spawn(uv_global_loop, &child_req, &options))) {
        fprintf(stderr, "uv_spawn error: (custom) %s\n", uv_strerror(r));
        // return 0;
    }


    uv_run(uv_global_loop, UV_RUN_DEFAULT);

   

    // uv_pipe_open(&stdOutPipe, 0);

    uv_read_start((uv_stream_t *)&stdOutPipe, alloc_buffer, on_read);
    uv_read_start((uv_stream_t *)&stdErrPipe, alloc_buffer, on_read);

    // wait for read to finish
    uv_run(uv_global_loop, UV_RUN_DEFAULT);

    uint64_t* texts[] = {
        string_to_addr(stdOutPipe.data),
        string_to_addr(stdErrPipe.data)
    };

    uint64_t* resultTexts = tuple_to_addr(2, texts);

    return resultTexts;

    // return unit_to_addr();
}


// void on_exit(uv_process_t *req, int64_t exit_status, int term_signal) {
//     fprintf(stderr, "Process exited with status %d, signal %d\n", exit_status, term_signal);
//     uv_close((uv_handle_t*) req, NULL);
// }

uint64_t* yyRunProcessSync(uint64_t* program, uint64_t* arguments)
{
    char* programName = addr_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    uint64_t** argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = string_to_addr(argumentArray[i]);
    }
    args[argumentCount+1]= NULL;


    uv_process_options_t options = {};

    options.file = args[0];
    options.args = args;
    // options.flags = 0;
    // options.exit_cb = on_exit;


    uv_process_t child_req;

    int r;
    if ((r = uv_spawn(uv_global_loop, &child_req, &options))) {
        fprintf(stderr, "uv_spawn [yyrunnooutput] error: (custom) %s\n", uv_strerror(r));
        // return 0;
    }


    uv_run(uv_global_loop, UV_RUN_DEFAULT);

    return unit_to_addr();
}
