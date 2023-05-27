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
        fprintf(stderr, "(error opening file ), %s\n", uv_strerror(open_req.result));
        fprintf(stderr, "when reading %s\n", addr_to_string(filenamearg));
        fflush(stderr);
        errorAndAbort("error printed");
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
        fprintf(stderr, "\n when reading %s", addr_to_string(filenamearg));
        fflush(stderr);
        errorAndAbort("error printed");
    }
    return string_to_addr(result);

}

// https://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
static void _mkdir(const char *dir) {
    char tmp[256];
    char *p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp),"%s",dir);
    len = strlen(tmp);
    if (tmp[len - 1] == '/')
        tmp[len - 1] = 0;
    for (p = tmp + 1; *p; p++)
        if (*p == '/') {
            *p = 0;
            mkdir(tmp, S_IRWXU);
            *p = '/';
        }
    mkdir(tmp, S_IRWXU);
}

yy_ptr yyWriteFileSync(yy_ptr file_name_addr, yy_ptr content_addr) {
    const char *filename = addr_to_string(file_name_addr);
    const char *content = addr_to_string(content_addr);
    uv_loop_t *loop = uv_default_loop();

    // // Create a new uv_fs_t structure for creating the directory
    // uv_fs_t mkdir_req;
    // memset(&mkdir_req, 0, sizeof(uv_fs_t));

    // Get the directory name
    char *directory_name = strdup(filename);
    char *last_slash = strrchr(directory_name, '/');
    if (last_slash != NULL) {
        *last_slash = '\0';
    }
    _mkdir(directory_name);

    // // Create the directory with permissions 0755
    // int result = uv_fs_mkdir(loop, &mkdir_req, directory_name, 0755, NULL);
    // if (result == UV_EEXIST) {
    //     // Directory already exists
    // } else if (result != 0) {
    //     // Failed to create directory
    //     fprintf(stderr, "Error creating directory: %s\ninput: %s\n", uv_strerror(result), filename);
    //     errorAndAbort("运行时错误");
    //     return unit_to_addr();
    // }

    // // Copy filename to a buffer so we can modify it
    // char* filename_copy = strdup(filename);
    // char* path = filename_copy;

    // // Create intermediate directories using strtok and mkdir
    // while ((path = strtok(path, "/"))) {
    //     // Skip empty tokens
    //     if (strlen(path) == 0) {
    //         continue;
    //     }

    //     // Append the next path component to the previous one
    //     char* last_slash = strrchr(filename_copy, '/');
    //     if (last_slash != NULL) {
    //         *last_slash = '\0';
    //     }
    //     strcat(filename_copy, "/");
    //     strcat(filename_copy, path);

    //     // Create the directory with permissions 0755
    //     int result = mkdir(filename_copy, 0755);
    //     if (result != 0 && errno != EEXIST) {
    //         // Failed to create directory
    //         fprintf(stderr, "Error creating directory: %s\n", strerror(errno));
    //         errorAndAbort("运行时错误");
    //         return unit_to_addr();;
    //     }

    //     // Advance to the next path component
    //     path = NULL;
    // }


    // Create a new uv_fs_t structure for writing the file
    uv_fs_t open_req;
    memset(&open_req, 0, sizeof(uv_fs_t));

    // Open the file for writing, with permissions 0644
    int result = uv_fs_open(loop, &open_req, filename, O_CREAT | O_WRONLY | O_TRUNC, 0644, NULL);
    if (result < 0) {
        // Failed to open file
        fprintf(stderr, "Error opening file: %s\n", uv_strerror(result));
        errorAndAbort("运行时错误");
        return unit_to_addr();;
    }

    // Write the content to the file
    uv_fs_t write_req;
    memset(&write_req, 0, sizeof(uv_fs_t));
    uv_buf_t buf = uv_buf_init((char*) content, strlen(content));
    int write_result = uv_fs_write(loop, &write_req, result, &buf, 1, -1, NULL);
    if (write_result < 0) {
        // Failed to write to file
        fprintf(stderr, "Error writing to file: %s\n", uv_strerror(write_result));
         errorAndAbort("运行时错误");
        return unit_to_addr();
    }

    // Close the file
    uv_fs_t close_req;
    memset(&close_req, 0, sizeof(uv_fs_t));
    result = uv_fs_close(loop, &close_req, result, NULL);
    if (result < 0) {
        // Failed to close file
        fprintf(stderr, "Error closing file: %s\n", uv_strerror(result));
        errorAndAbort("运行时错误");
        return unit_to_addr();    
    }

    return unit_to_addr();
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
        entries[nread] = string_to_addr(strdup(name));
        nread++;
    }



    return array_to_iso_addr(nread, entries);
}

yy_ptr yyIsPathDirectory(yy_ptr path){

    uv_fs_t req;

    char * pathC = addr_to_string(path);

  int r = uv_fs_stat(NULL, &req, pathC, NULL);
  if (r != 0){
      fprintf(stderr, " cannot stat directory - %s - \n", pathC);
      errorAndAbort("cannot stat directory!!!!");
  }
  bool isdir = (((uv_stat_t*)req.ptr)->st_mode & S_IFDIR);

  return bool_to_addr(isdir);


}

yy_ptr yyIsPathRegularFile(yy_ptr path){

    uv_fs_t req;

    char * pathC = addr_to_string(path);

  int r = uv_fs_stat(NULL, &req, pathC, NULL);
  if (r != 0){
      fprintf(stderr, " cannot stat file");
      errorAndAbort("cannot stat file!!!");
  }
  bool isfile = (((uv_stat_t*)req.ptr)->st_mode & S_IFREG);

  return bool_to_addr(isfile);
}

yy_ptr yyGetFileModifiedTime(yy_ptr path){

    uv_fs_t req;

    char * pathC = addr_to_string(path);

  int r = uv_fs_stat(NULL, &req, pathC, NULL);
  if (r != 0){
      fprintf(stderr, " cannot stat file");
      errorAndAbort("cannot stat file!!!");
  }
  int64_t mtime = (((uv_stat_t*)req.ptr)->st_mtim).tv_sec;

  return int_to_addr(mtime);
}

yy_ptr yyPathExists(yy_ptr path){
    uv_fs_t req;

    char * pathC = addr_to_string(path);

    int r = uv_fs_stat(NULL, &req, pathC, NULL);

    if (r != 0){
        return bool_to_addr(false);
    }

    return bool_to_addr(true);
}