
#include "../公共包含.h"


豫言值 豫言_运行于Windows(){
#ifdef _WIN32
    return 爻转豫言值(true);
#else
    return 爻转豫言值(false);
#endif
}

豫言值 豫言_运行于MacOS(){
#ifdef __APPLE__
    return 爻转豫言值(true);
#else
    return 爻转豫言值(false);
#endif
}

豫言值 豫言_运行于Linux(){
#ifdef __linux__
    return 爻转豫言值(true);
#else
    return 爻转豫言值(false);
#endif
}
