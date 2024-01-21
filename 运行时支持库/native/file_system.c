#include "common_include.h"

yyvalue yyReadFileSync(yyvalue filenamearg) {
    const char *filename = yyvalue_to_string(filenamearg);
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

    return string_to_yyvalue(result);
}

yyvalue yyDeleteFileSync(yyvalue filenamearg) {
    const char *filename = yyvalue_to_string(filenamearg);
    int result = remove(filename);
    if (result != 0) {
        fprintf(stderr, "Error deleting file: %s\n", filename);
        errorAndAbort("error printed");
    }

    return unit_to_yyvalue();
}


/* Make a directory; already existing dir okay */
static int maybe_mkdir(const char* path, mode_t mode)
{
    struct stat st;
    errno = 0;

    /* Try to make the directory */
    if (mkdir(path, mode) == 0)
        return 0;

    /* If it fails for any reason but EEXIST, fail */
    if (errno != EEXIST)
        return -1;

    /* Check if the existing path is a directory */
    if (stat(path, &st) != 0)
        return -1;

    /* If not, fail with ENOTDIR */
    if (!S_ISDIR(st.st_mode)) {
        errno = ENOTDIR;
        return -1;
    }

    errno = 0;
    return 0;
}

int mkdir_p(const char *path)
{
    /* Adapted from http://stackoverflow.com/a/2336245/119527 */
    char *_path = NULL;
    char *p; 
    int result = -1;
    mode_t mode = 0777;

    errno = 0;

    /* Copy string so it's mutable */
    _path = strdup(path);
    if (_path == NULL)
        goto out;

    /* Iterate the string */
    for (p = _path + 1; *p; p++) {
        if (*p == '/') {
            /* Temporarily truncate */
            *p = '\0';

            if (maybe_mkdir(_path, mode) != 0)
                goto out;

            *p = '/';
        }
    }   

    if (maybe_mkdir(_path, mode) != 0)
        goto out;

    result = 0;

out:
    free(_path);
    return result;
}
yyvalue yyWriteFileSync(yyvalue file_name_addr, yyvalue content_addr) {
    const char *filename = yyvalue_to_string(file_name_addr);
    const char *content = yyvalue_to_string(content_addr);

    char *directory_name = strdup(filename);
    char *last_slash = strrchr(directory_name, '/');
    if (last_slash != NULL) {
        *last_slash = '\0';
        if (mkdir_p(directory_name) != 0){
            fprintf(stderr, "Error creating directory (-p): %s\n", filename);
            errorAndAbort("运行时错误");
        }
    }
    free(directory_name);

    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        errorAndAbort("运行时错误");
        return unit_to_yyvalue();
    }

    if (fputs(content, file) == EOF) {
        fprintf(stderr, "Error writing to file: %s\n", filename);
        errorAndAbort("运行时错误");
        fclose(file);
        return unit_to_yyvalue();
    }

    fclose(file);
    return unit_to_yyvalue();
}


// TODO: we should probably make the following functions windows compatible, i.e. use libuv specifically on windows
yyvalue yyListDirectorySync(yyvalue dirname) {
    char *dname = yyvalue_to_string(dirname);
    DIR *dir = opendir(dname);
    if (dir == NULL) {
        fprintf(stderr, "Error opening directory: %s\n", dname);
        fflush(stderr);
        return unit_to_yyvalue();
    }

    struct dirent *entry;
    yyvalue entries[4096];  // Assuming a maximum of 4096 entries in the directory
    int nread = 0;

    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        entries[nread] = string_to_yyvalue(strdup(name));
        nread++;
    }

    closedir(dir);

    return array_to_iso_addr(nread, entries);
}


yyvalue yyIsPathDirectory(yyvalue path) {
    char *pathC = yyvalue_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        fprintf(stderr, "Cannot stat directory: %s\n", pathC);
        errorAndAbort("Cannot stat directory!!!!");
    }

    bool isdir = S_ISDIR(st.st_mode);

    return bool_to_yyvalue(isdir);
}

yyvalue yyIsPathRegularFile(yyvalue path) {
    char *pathC = yyvalue_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        fprintf(stderr, "Cannot stat file: %s\n", pathC);
        errorAndAbort("Cannot stat file!!!");
    }

    bool isfile = S_ISREG(st.st_mode);

    return bool_to_yyvalue(isfile);
}

yyvalue yyPathExists(yyvalue path) {
    char *pathC = yyvalue_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        return bool_to_yyvalue(false);
    }

    return bool_to_yyvalue(true);
}

yyvalue yyGetFileModifiedTime(yyvalue path) {
    char *pathC = yyvalue_to_string(path);

    struct stat st;
    if (stat(pathC, &st) != 0) {
        fprintf(stderr, "Cannot stat file: %s", pathC);
        errorAndAbort("Cannot stat file!!!");
    }
    int64_t mtime = st.st_mtime;

    return int_to_yyvalue(mtime);
}


yyvalue yyGetCurrentWorkingDirectory() {
    char* path_buffer = yy_gcAllocateBytes(PATH_MAX * sizeof(char));
    if (path_buffer == NULL) {
        errorAndAbort("Cannot allocate memory for path buffer!!!");
    }

    if (getcwd(path_buffer, PATH_MAX) == NULL) {
        errorAndAbort("Cannot get current working directory!!!");
    }

    return string_to_yyvalue(path_buffer);
}