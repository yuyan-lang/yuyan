#include "../globalInclude.h"


yy_ptr yyReadFileSync(yy_ptr filenamearg) {
    uv_fs_t open_req;
    uv_fs_t read_req;
    // open_req = GC_MALLOC(sizeof(uv_fs_t))

    int openResult = uv_fs_open(uv_default_loop(), &open_req, 
        addr_to_string(filenamearg), O_RDONLY, 
        0, NULL);

    if (openResult < 0 ){
        // error happpened
    }
    char* result = GC_MALLOC(1);
    int resultLength = 0;
    result[0] = '\0';
    char* tempBuffer = GC_MALLOC(65535);
    uv_buf_t uvBuf= uv_buf_init(tempBuffer, 65534);
    
    int nread = uv_fs_read(uv_default_loop(), &read_req, open_req.result, &uvBuf, 1, -1, NULL);
    while (read_req.result > 0) {
        resultLength += nread;
        result = GC_REALLOC(result, resultLength +1);
        tempBuffer[nread] = '\0';
        strlcat(result, tempBuffer, resultLength+1);
        nread = uv_fs_read(uv_default_loop(), &read_req, open_req.result, &uvBuf, 1, -1, NULL);
    }
    if (read_req.result == UV_EOF | read_req.result == 0){
        // done
    } else {
        fprintf(stderr, "(error reading file ), %s", uv_strerror(read_req.result));
        fflush(stderr);
    }
    return string_to_addr(result);

}


yy_ptr yyListDirectorySync(yy_ptr dirname) {
    uv_fs_t scan_req;
    // uv_fs_t read_req;
    // open_req = GC_MALLOC(sizeof(uv_fs_t))

char * dname = addr_to_string(dirname);
    int count = uv_fs_scandir(uv_default_loop(), &scan_req, dname
        , O_RDONLY, NULL);


    if (count < 0 ){
        // error happpened
        fprintf(stderr, "(error reading dir ), %s", uv_strerror(scan_req.result));
        fflush(stderr);
    }

    uv_dirent_t dirs;
    yy_ptr entries[count];
    int nread=0;

    while(uv_fs_scandir_next(&scan_req, &dirs) != UV_EOF){
        const char * name = dirs.name;
        entries[nread] = string_to_addr(name);
        nread++;
    }



    return array_to_iso_addr(nread, entries);
}

yy_ptr yyIsPathDirectory(yy_ptr path){

    uv_fs_t req;

    char * pathC = addr_to_string(path);

  int r = uv_fs_stat(NULL, &req, pathC, NULL);
  if (r != 0){
      fprintf(stderr, " cannot stat directory");
  }
  bool isdir = (((uv_stat_t*)req.ptr)->st_mode & S_IFDIR);

  return bool_to_addr(isdir);


}