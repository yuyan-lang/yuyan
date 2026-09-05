#include "公共包含.h"
#include <fcntl.h>
#include <sys/file.h>

// 文言：取实径以定界，察链接而不循之。汉语：包发现使用真实路径和 lstat，避免符号链接绕过边界。
豫言值 豫言_路径为符号链接(豫言值 路径值) {
    struct stat 状态;
    if (lstat(豫言值转字符串(路径值), &状态) != 0) {
        if (errno == ENOENT) return 爻转豫言值(false);
        报错并中止("无法检查路径的符号链接状态");
    }
    return 爻转豫言值(S_ISLNK(状态.st_mode));
}

豫言值 豫言_取得真实路径(豫言值 路径值) {
    char *路径 = realpath(豫言值转字符串(路径值), NULL);
    if (路径 == NULL) 报错并中止("无法取得真实路径");
    豫言值 结果 = 复制字符串为豫言值(strlen(路径) + 1, 路径);
    free(路径);
    return 结果;
}

// 文言：同文用旧号，异文另立号，既存不改。汉语：在当前工具链目录保存不可变上下文；逐字比较去重，避免摘要碰撞或覆盖旧缓存。
豫言值 豫言_存放包上下文(豫言值 内容值) {
    const char *内容 = 豫言值转字符串(内容值);
    const char *目录们[] = {".yybuild", ".yybuild/豫构上下文"};
    for (int 序 = 0; 序 < 2; ++序) {
        if (mkdir(目录们[序], 0700) != 0 && errno != EEXIST) 报错并中止("无法创建包上下文目录");
        struct stat 状态;
        if (lstat(目录们[序], &状态) != 0 || !S_ISDIR(状态.st_mode)) 报错并中止("包上下文目录不是普通目录");
    }
    int 锁 = open(".yybuild/豫构上下文/锁", O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (锁 < 0) 报错并中止("无法打开包上下文锁");
    if (flock(锁, LOCK_EX) != 0) { close(锁); 报错并中止("无法取得包上下文锁"); }
    char 路径[160];
    size_t 长度 = strlen(内容);
    for (uint64_t 号 = 1; ; ++号) {
        snprintf(路径, sizeof(路径), ".yybuild/豫构上下文/%" PRIu64 ".上下文", 号);
        int 文件 = open(路径, O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
        if (文件 >= 0) {
            struct stat 状态;
            bool 相同 = fstat(文件, &状态) == 0 && S_ISREG(状态.st_mode) && 状态.st_size == (off_t)长度;
            size_t 位置 = 0;
            char 块[4096];
            while (相同 && 位置 < 长度) {
                ssize_t 数 = read(文件, 块, sizeof(块));
                if (数 < 0 && errno == EINTR) continue;
                if (数 <= 0 || (size_t)数 > 长度 - 位置 || memcmp(块, 内容 + 位置, (size_t)数) != 0) { 相同 = false; break; }
                位置 += (size_t)数;
            }
            close(文件);
            if (相同) break;
            continue;
        }
        if (errno != ENOENT) { close(锁); 报错并中止("无法读取包上下文"); }
        文件 = open(路径, O_CREAT | O_EXCL | O_WRONLY | O_CLOEXEC | O_NOFOLLOW, 0600);
        if (文件 < 0) { close(锁); 报错并中止("无法创建包上下文"); }
        size_t 位置 = 0;
        while (位置 < 长度) {
            ssize_t 数 = write(文件, 内容 + 位置, 长度 - 位置);
            if (数 < 0 && errno == EINTR) continue;
            if (数 <= 0) { close(文件); unlink(路径); close(锁); 报错并中止("无法写入包上下文"); }
            位置 += (size_t)数;
        }
        if (close(文件) != 0) { unlink(路径); close(锁); 报错并中止("无法保存包上下文"); }
        break;
    }
    close(锁);
    char *实径 = realpath(路径, NULL);
    if (实径 == NULL) 报错并中止("无法定位包上下文");
    豫言值 结果 = 复制字符串为豫言值(strlen(实径) + 1, 实径);
    free(实径);
    return 结果;
}

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

    豫言值 返回值 = 复制字符串为豫言值(读取大小 + 1, 结果);
    free(结果);
    return 返回值;
}

豫言值 豫言_同步读取文件字节串(豫言值 文件名参数) {
    const char *文件名 = 豫言值转字符串(文件名参数);
    FILE *文件 = fopen(文件名, "rb");
    if (文件 == NULL) {
        fprintf(stderr, "无法打开文件：%s\n", 文件名);
        报错并中止("读取文件字节串失败");
    }
    if (fseek(文件, 0, SEEK_END) != 0) {
        fclose(文件);
        报错并中止("读取文件字节串时无法确定文件大小");
    }
    long 文件大小 = ftell(文件);
    if (文件大小 < 0 || (uint64_t)文件大小 > UINT32_MAX) {
        fclose(文件);
        报错并中止("文件过大，无法读入字节串");
    }
    if (fseek(文件, 0, SEEK_SET) != 0) {
        fclose(文件);
        报错并中止("读取文件字节串时无法回到文件开头");
    }

    豫言值 返回值 = 分配豫言_字节串缓冲区((uint64_t)文件大小);
    unsigned char *结果 = 豫言值转字节串指针(返回值);
    size_t 已读取 = 0;
    while (已读取 < (size_t)文件大小) {
        size_t 本次读取 = fread(结果 + 已读取, 1, (size_t)文件大小 - 已读取, 文件);
        if (本次读取 == 0) {
            fclose(文件);
            报错并中止("读取文件字节串时未能读完整个文件");
        }
        已读取 += 本次读取;
    }
    if (fclose(文件) != 0) {
        报错并中止("关闭已读取文件失败");
    }
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

豫言值 豫言_同步写入文件字节串(豫言值 文件名地址, 豫言值 内容地址) {
    const char *文件名 = 豫言值转字符串(文件名地址);
    const unsigned char *内容 = 豫言值转字节串指针(内容地址);
    uint64_t 内容长度 = 获取豫言_字节串长度(内容地址);

    char *文件夹名 = strdup(文件名);
    if (文件夹名 == NULL) {
        报错并中止("写入文件字节串时无法复制文件名");
    }
    char *最后斜线 = strrchr(文件夹名, '/');
    if (最后斜线 != NULL) {
        *最后斜线 = '\0';
        if (递归创建文件夹(文件夹名) != 0){
            free(文件夹名);
            fprintf(stderr, "无法递归创建文件夹：%s\n", 文件名);
            报错并中止("创建文件夹失败");
        }
    }
    free(文件夹名);

    FILE *文件 = fopen(文件名, "wb");
    if (文件 == NULL) {
        fprintf(stderr, "无法打开文件：%s\n", 文件名);
        报错并中止("写入文件字节串失败");
    }

    uint64_t 已写入 = 0;
    while (已写入 < 内容长度) {
        size_t 本次写入 = fwrite(内容 + 已写入, 1, 内容长度 - 已写入, 文件);
        if (本次写入 == 0) {
            fclose(文件);
            fprintf(stderr, "无法写入文件：%s\n", 文件名);
            报错并中止("写入文件字节串失败");
        }
        已写入 += 本次写入;
    }
    if (fclose(文件) != 0) {
        报错并中止("关闭已写入文件失败");
    }
    return 单元转豫言值();
}


// 待办：为下列函数提供 Windows 实现。
豫言值 豫言_同步列出文件夹(豫言值 文件夹名) {
    char *文件夹名文本 = 豫言值转字符串(文件夹名);
    DIR *文件夹句柄 = opendir(文件夹名文本);
    if (文件夹句柄 == NULL) {
        fprintf(stderr, "无法打开文件夹：%s\n", 文件夹名文本);
        fflush(stderr);
        报错并中止("无法读取文件夹");
    }

    struct dirent *条目;
    // 文言：项多则益其容，不越所储。汉语：包发现可能扫描大目录，不使用固定大小的栈缓冲区。
    size_t 容量 = 32;
    豫言值 *条目组 = malloc(容量 * sizeof(豫言值));
    if (条目组 == NULL) { closedir(文件夹句柄); 报错并中止("无法分配目录条目"); }
    size_t 读取数量 = 0;

    errno = 0;
    while ((条目 = readdir(文件夹句柄)) != NULL) {
        if (读取数量 == 容量) {
            if (容量 > SIZE_MAX / sizeof(豫言值) / 2) { free(条目组); closedir(文件夹句柄); 报错并中止("目录条目过多"); }
            容量 *= 2;
            豫言值 *新组 = realloc(条目组, 容量 * sizeof(豫言值));
            if (新组 == NULL) { free(条目组); closedir(文件夹句柄); 报错并中止("无法扩展目录条目"); }
            条目组 = 新组;
        }
        const char *名称 = 条目->d_name;
        条目组[读取数量] = 复制字符串为豫言值(strlen(名称) + 1, 名称);
        读取数量++;
        errno = 0;
    }
    int 读取错误 = errno;
    closedir(文件夹句柄);
    if (读取错误 != 0) { free(条目组); 报错并中止("读取目录条目失败"); }
    豫言值 结果 = 数组转同构列(读取数量, 条目组);
    free(条目组);
    return 结果;
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

豫言值 豫言_切换当前工作目录(豫言值 路径) {
    const char *路径文本 = 豫言值转字符串(路径);
    if (chdir(路径文本) == 0) {
        豫言值 值组[] = {
            整数转豫言值(0),
            复制字节为豫言值(0, ""),
        };
        return 元组转豫言值(2, 值组);
    }

    int 状态 = errno;
    const char *错误 = strerror(状态);
    豫言值 值组[] = {
        整数转豫言值(状态),
        复制字节为豫言值(strlen(错误), 错误),
    };
    return 元组转豫言值(2, 值组);
}

豫言值 豫言_路径可执行(豫言值 路径) {
    const char *路径文本 = 豫言值转字符串(路径);
    struct stat 状态信息;
    bool 可执行 = stat(路径文本, &状态信息) == 0
        && S_ISREG(状态信息.st_mode)
        && access(路径文本, X_OK) == 0;
    return 爻转豫言值(可执行);
}

static bool 文本路径可执行(const char *路径) {
    struct stat 状态信息;
    return stat(路径, &状态信息) == 0
        && S_ISREG(状态信息.st_mode)
        && access(路径, X_OK) == 0;
}

static 豫言值 可执行搜索结果(bool 找到, const char *路径) {
    豫言值 值组[] = {
        爻转豫言值(找到),
        复制字节为豫言值(找到 ? strlen(路径) : 0, 找到 ? 路径 : ""),
    };
    return 元组转豫言值(2, 值组);
}

豫言值 豫言_查找可执行程序(豫言值 程序名) {
    const char *名称 = 豫言值转字符串(程序名);
    if (strchr(名称, '/') != NULL) {
        return 可执行搜索结果(文本路径可执行(名称), 名称);
    }

    size_t 名称长度 = strlen(名称);
    char *本地路径 = malloc(名称长度 + 3);
    if (本地路径 == NULL) {
        报错并中止("查找可执行程序时无法分配内存");
    }
    memcpy(本地路径, "./", 2);
    memcpy(本地路径 + 2, 名称, 名称长度 + 1);
    if (文本路径可执行(本地路径)) {
        豫言值 结果 = 可执行搜索结果(true, 本地路径);
        free(本地路径);
        return 结果;
    }
    free(本地路径);

    const char *环境路径 = getenv("PATH");
    if (环境路径 == NULL) 环境路径 = "/usr/local/bin:/usr/bin:/bin";
    const char *当前 = 环境路径;
    for (;;) {
        const char *分隔处 = strchr(当前, ':');
        size_t 目录长度 = 分隔处 == NULL ? strlen(当前) : (size_t)(分隔处 - 当前);
        const char *目录 = 当前;
        if (目录长度 == 0) {
            目录 = ".";
            目录长度 = 1;
        }
        if (目录长度 <= SIZE_MAX - 名称长度 - 2) {
            size_t 路径长度 = 目录长度 + 1 + 名称长度;
            char *路径 = malloc(路径长度 + 1);
            if (路径 == NULL) {
                报错并中止("查找可执行程序时无法分配内存");
            }
            memcpy(路径, 目录, 目录长度);
            路径[目录长度] = '/';
            memcpy(路径 + 目录长度 + 1, 名称, 名称长度 + 1);
            if (文本路径可执行(路径)) {
                豫言值 结果 = 可执行搜索结果(true, 路径);
                free(路径);
                return 结果;
            }
            free(路径);
        }
        if (分隔处 == NULL) break;
        当前 = 分隔处 + 1;
    }
    return 可执行搜索结果(false, "");
}
