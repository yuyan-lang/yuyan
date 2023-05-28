// #include "../native_include.h"

// #include "limits.h"

// char* yyGetCurrentWorkingDirectory(){
//     size_t sz = PATH_MAX * sizeof(char);
//     char *path_buffer = yy_gcAllocateBytes(sz);
//     int err = uv_cwd(path_buffer, &sz);
//     if (err != 0){
//         errorAndAbort("Cannot get CWD, SIZE ERROR!!!");
//     }
//     return path_buffer;
// }