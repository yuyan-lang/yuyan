
#include "../native_include.h"

void on_alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
  buf->base = yy_gcAllocateBytes(suggested_size);
  buf->len = suggested_size;
}

//https://github.com/libuv/help/issues/22
void on_read(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf) {
     if (nread < 0) {
         if (nread != UV_EOF)
             fprintf(stderr, "[uvhelper] Read error %s\n", uv_err_name(nread));
         uv_close((uv_handle_t*) client, NULL);
         return;
     }
 
     char* newData = (char*) yy_gcAllocateBytes(sizeof(char) * (nread+1));
     newData[nread] = '\0';
     strncpy(newData, buf->base, nread);
    
    if (client->data == NULL ) {
        client->data = newData;
    } else {
        char *oldData = client->data;
        int oldDataLength = strlen(oldData);
        int newLength = (nread + oldDataLength + 1);
        client->data = GC_REALLOC(client->data, sizeof(char) * newLength);
        strlcat(client->data, newData, newLength);
    }


 
    //  fprintf(stderr, "%s", data);
 }

void readStreamUntilEofIntoDataAync(uv_stream_t *stream){
    stream->data = NULL;
    uv_read_start(stream, on_alloc_buffer, on_read);
}