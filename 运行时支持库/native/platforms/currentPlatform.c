
#include "../common_include.h"


yyvalue yyRunningOnWindows(){
#ifdef _WIN32
    return bool_to_yyvalue(true);
#else
    return bool_to_yyvalue(false);
#endif
}

yyvalue yyRunningOnMacOS(){
#ifdef __APPLE__
    return bool_to_yyvalue(true);
#else
    return bool_to_yyvalue(false);
#endif
}

yyvalue yyRunningOnLinux(){
#ifdef __linux__
    return bool_to_yyvalue(true);
#else
    return bool_to_yyvalue(false);
#endif
}