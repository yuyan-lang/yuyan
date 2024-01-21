
#include "../native_include.h"

void on_alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf) {
  buf->base = malloc(suggested_size);
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
 
     char* newData = (char*) malloc(sizeof(char) * (nread+1));
     memcpy(newData, buf->base, nread);
     newData[nread] = '\0';
    
    if (client->data == NULL ) {
        client->data = newData;
    } else {
        char *oldData = client->data;
        int oldDataLength = strlen(oldData);
        int newLength = (nread + oldDataLength + 1);
        client->data = realloc(client->data, sizeof(char) * newLength);
        strncat(client->data, newData, nread);
    }


 
    //  fprintf(stderr, "%s", data);
 }

void readStreamUntilEofIntoDataAync(uv_stream_t *stream){
    stream->data = (char*)malloc(1);  // Allocate memory for an empty string (null terminator)
    strcpy(stream->data, "");
    uv_read_start(stream, on_alloc_buffer, on_read);
}