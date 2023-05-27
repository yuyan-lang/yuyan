
#include <stdbool.h>


bool yyRunningOnWindows(){
#ifdef _WIN32
    return true;
#else
    return false;
#endif
}

bool yyRunningOnMacOS(){
#ifdef __APPLE__
    return true;
#else
    return false;
#endif
}

bool yyRunningOnLinux(){
#ifdef __linux__
    return true;
#else
    return false;
#endif
}