#include "common_include.h"

yy_ptr yyReadFileSync(yy_ptr filenamearg) {
    const char *filename = addr_to_string(filenamearg);
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        errorAndAbort("error printed");
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char *result = (char *)yy_gcAllocateBytes(file_size + 1);
    if (result == NULL) {
        fclose(file);
        fprintf(stderr, "Memory allocation error\n");
        errorAndAbort("error printed");
    }

    size_t read_size = fread(result, 1, file_size, file);
    result[read_size] = '\0';

    fclose(file);

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

    char *directory_name = strdup(filename);
    char *last_slash = strrchr(directory_name, '/');
    if (last_slash != NULL) {
        *last_slash = '\0';
        _mkdir(directory_name);
    }
    free(directory_name);

    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        errorAndAbort("运行时错误");
        return unit_to_addr();
    }

    if (fputs(content, file) == EOF) {
        fprintf(stderr, "Error writing to file: %s\n", filename);
        errorAndAbort("运行时错误");
        fclose(file);
        return unit_to_addr();
    }

    fclose(file);
    return unit_to_addr();
}


// TODO: we should probably make the following functions windows compatible, i.e. use libuv specifically on windows
yy_ptr yyListDirectorySync(yy_ptr dirname) {
    char *dname = addr_to_string(dirname);
    DIR *dir = opendir(dname);
    if (dir == NULL) {
        fprintf(stderr, "Error opening directory: %s\n", dname);
        fflush(stderr);
        return unit_to_addr();
    }

    struct dirent *entry;
    yy_ptr entries[4096];  // Assuming a maximum of 4096 entries in the directory
    int nread = 0;

    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        entries[nread] = string_to_addr(strdup(name));
        nread++;
    }

    closedir(dir);

    return array_to_iso_addr(nread, entries);
}


yy_ptr yyIsPathDirectory(yy_ptr path) {
    char *pathC = addr_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        fprintf(stderr, "Cannot stat directory: %s\n", pathC);
        errorAndAbort("Cannot stat directory!!!!");
    }

    bool isdir = S_ISDIR(st.st_mode);

    return bool_to_addr(isdir);
}

yy_ptr yyIsPathRegularFile(yy_ptr path) {
    char *pathC = addr_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        fprintf(stderr, "Cannot stat file: %s\n", pathC);
        errorAndAbort("Cannot stat file!!!");
    }

    bool isfile = S_ISREG(st.st_mode);

    return bool_to_addr(isfile);
}

yy_ptr yyPathExists(yy_ptr path) {
    char *pathC = addr_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        return bool_to_addr(false);
    }

    return bool_to_addr(true);
}

yy_ptr yyGetFileModifiedTime(yy_ptr path) {
    char *pathC = addr_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        fprintf(stderr, "Cannot stat file: %s", pathC);
        errorAndAbort("Cannot stat file!!!");
    }
    int64_t mtime = st.st_mtime;

    return int_to_addr(mtime);
}


char* yyGetCurrentWorkingDirectory() {
    char* path_buffer = malloc(PATH_MAX * sizeof(char));
    if (path_buffer == NULL) {
        errorAndAbort("Cannot allocate memory for path buffer!!!");
    }

    if (getcwd(path_buffer, PATH_MAX) == NULL) {
        errorAndAbort("Cannot get current working directory!!!");
    }

    return path_buffer;
}