#include "公共包含.h"

豫言值 豫言_同步读取文件(豫言值 文件名参数) {
    const char *文件名 = 豫言值转字符串(文件名参数);
    FILE *文件 = fopen(文件名, "rb");
    if (文件 == NULL) {
        fprintf(stderr, "无法打开文件：%s\n", 文件名);
        报错并中止("读取文件失败");
    }

    fseek(文件, 0, SEEK_END);
    long 文件大小 = ftell(文件);
    rewind(文件);

    char *结果 = malloc(文件大小 + 1);
    if (结果 == NULL) {
        fclose(文件);
        报错并中止("读取文件时无法分配内存");
    }

    size_t 读取大小 = fread(结果, 1, 文件大小, 文件);
    结果[读取大小] = '\0';

    fclose(文件);

    豫言值 返回值 = 复制字符串为豫言值(文件大小 + 1, 结果);
    free(结果);
    return 返回值;
}

豫言值 豫言_同步删除文件(豫言值 文件名参数) {
    const char *文件名 = 豫言值转字符串(文件名参数);
    int 结果 = remove(文件名);
    if (结果 != 0) {
        fprintf(stderr, "无法删除文件：%s\n", 文件名);
        报错并中止("删除文件失败");
    }

    return 单元转豫言值();
}


/* 创建文件夹；目标已经是文件夹时也视为成功。 */
static int 尝试创建文件夹(const char* 路径, mode_t 模式)
{
    struct stat 状态信息;
    errno = 0;

    /* 尝试创建文件夹。 */
    if (mkdir(路径, 模式) == 0)
        return 0;

    /* 除“已经存在”外，其余错误均直接失败。 */
    if (errno != EEXIST)
        return -1;

    /* 检查已经存在的路径是否为文件夹。 */
    if (stat(路径, &状态信息) != 0)
        return -1;

    /* 如果不是文件夹，则以相应错误码失败。 */
    if (!S_ISDIR(状态信息.st_mode)) {
        errno = ENOTDIR;
        return -1;
    }

    errno = 0;
    return 0;
}

int 递归创建文件夹(const char *路径)
{
    char *可修改路径 = NULL;
    char *扫描指针;
    int 结果 = -1;
    mode_t 模式 = 0777;

    errno = 0;

    /* 复制出可修改的路径。 */
    可修改路径 = strdup(路径);
    if (可修改路径 == NULL)
        goto 输出;

    /* 逐段创建路径。 */
    for (扫描指针 = 可修改路径 + 1; *扫描指针; 扫描指针++) {
        if (*扫描指针 == '/') {
            *扫描指针 = '\0';

            if (尝试创建文件夹(可修改路径, 模式) != 0)
                goto 输出;

            *扫描指针 = '/';
        }
    }

    if (尝试创建文件夹(可修改路径, 模式) != 0)
        goto 输出;

    结果 = 0;

输出:
    free(可修改路径);
    return 结果;
}
豫言值 豫言_同步写入文件(豫言值 文件名地址, 豫言值 内容地址) {
    const char *文件名 = 豫言值转字符串(文件名地址);
    const char *内容 = 豫言值转字符串(内容地址);

    char *文件夹名 = strdup(文件名);
    char *最后斜线 = strrchr(文件夹名, '/');
    if (最后斜线 != NULL) {
        *最后斜线 = '\0';
        if (递归创建文件夹(文件夹名) != 0){
            fprintf(stderr, "无法递归创建文件夹：%s\n", 文件名);
            报错并中止("创建文件夹失败");
        }
    }
    free(文件夹名);

    FILE *文件 = fopen(文件名, "w");
    if (文件 == NULL) {
        fprintf(stderr, "无法打开文件：%s\n", 文件名);
        报错并中止("写入文件失败");
        return 单元转豫言值();
    }

    if (fputs(内容, 文件) == EOF) {
        fprintf(stderr, "无法写入文件：%s\n", 文件名);
        报错并中止("写入文件失败");
        fclose(文件);
        return 单元转豫言值();
    }

    fclose(文件);
    return 单元转豫言值();
}


// 待办：为下列函数提供 Windows 实现。
豫言值 豫言_同步列出文件夹(豫言值 文件夹名) {
    char *文件夹名文本 = 豫言值转字符串(文件夹名);
    DIR *文件夹句柄 = opendir(文件夹名文本);
    if (文件夹句柄 == NULL) {
        fprintf(stderr, "无法打开文件夹：%s\n", 文件夹名文本);
        fflush(stderr);
        return 单元转豫言值();
    }

    struct dirent *条目;
    豫言值 条目组[4096];  // 当前最多读取四千零九十六个条目。
    int 读取数量 = 0;

    while ((条目 = readdir(文件夹句柄)) != NULL) {
        const char *名称 = 条目->d_name;
        条目组[读取数量] = 复制字符串为豫言值(strlen(名称) + 1, 名称);
        读取数量++;
    }

    closedir(文件夹句柄);

    return 数组转同构列(读取数量, 条目组);
}


豫言值 豫言_路径是文件夹(豫言值 路径) {
    char *路径文本 = 豫言值转字符串(路径);

    struct stat 状态信息;
    if (stat(路径文本, &状态信息) != 0) {
        fprintf(stderr, "无法读取路径状态：%s\n", 路径文本);
        报错并中止("判断路径是否为文件夹失败");
    }

    bool 是文件夹 = S_ISDIR(状态信息.st_mode);

    return 爻转豫言值(是文件夹);
}

豫言值 豫言_路径是普通文件(豫言值 路径) {
    char *路径文本 = 豫言值转字符串(路径);

    struct stat 状态信息;
    if (stat(路径文本, &状态信息) != 0) {
        fprintf(stderr, "无法读取路径状态：%s\n", 路径文本);
        报错并中止("判断路径是否为普通文件失败");
    }

    bool 是普通文件 = S_ISREG(状态信息.st_mode);

    return 爻转豫言值(是普通文件);
}

豫言值 豫言_路径存在(豫言值 路径) {
    char *路径文本 = 豫言值转字符串(路径);

    struct stat 状态信息;
    if (stat(路径文本, &状态信息) != 0) {
        return 爻转豫言值(false);
    }

    return 爻转豫言值(true);
}

豫言值 豫言_获取文件修改时间(豫言值 路径) {
    char *路径文本 = 豫言值转字符串(路径);

    struct stat 状态信息;
    if (stat(路径文本, &状态信息) != 0) {
        fprintf(stderr, "无法读取文件状态：%s\n", 路径文本);
        报错并中止("获取文件修改时间失败");
    }
    int64_t 修改时间 = 状态信息.st_mtime;

    return 整数转豫言值(修改时间);
}


豫言值 豫言_获取当前工作目录() {
    char* 路径缓冲区 = malloc(PATH_MAX * sizeof(char));
    if (路径缓冲区 == NULL) {
        报错并中止("无法为当前工作目录分配路径缓冲区");
    }

    if (getcwd(路径缓冲区, PATH_MAX) == NULL) {
        报错并中止("无法获取当前工作目录");
    }

    豫言值 返回值 = 复制字符串为豫言值(strlen(路径缓冲区) + 1, 路径缓冲区);
    free(路径缓冲区);
    return 返回值;
}
