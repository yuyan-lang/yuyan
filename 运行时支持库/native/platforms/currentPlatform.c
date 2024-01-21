
#include <stdbool.h>
#include <stdint.h>


uint64_t yyRunningOnWindows(){
#ifdef _WIN32
    return true;
#else
    return false;
#endif
}

uint64_t yyRunningOnMacOS(){
#ifdef __APPLE__
    return true;
#else
    return false;
#endif
}

uint64_t yyRunningOnLinux(){
#ifdef __linux__
    return true;
#else
    return false;
#endif
}