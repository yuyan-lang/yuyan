#include "globalInclude.h"


uint64_t* yyReadFileSync(uint64_t* filenamearg) {
    uv_fs_t open_req;
    uv_fs_t read_req;
    // open_req = GC_MALLOC(sizeof(uv_fs_t))

    int result = uv_fs_open(uv_default_loop(), &open_req, 
        addr_to_string(filenamearg), O_RDONLY, 
        0, NULL);

    if (result < 0 ){
        // error happpened
    }
    char* buffer = GC_MALLOC(1000000);
    uv_buf_t uvBuf= uv_buf_init(buffer, 1000000 * 8);
    
    int ret = uv_fs_read(uv_default_loop(), &read_req, open_req.result, &uvBuf, 1, -1, NULL);
    if (ret < 0 ){
        // error happened
    }
    else if (ret > 0) {
        // error : file is too large
    }

    return string_to_addr(buffer);
}