#include "公共包含.h"
#include <fcntl.h>
#include <sys/file.h>

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

/* 文言：同事加锁，毕事释之。汉语：豫构对不可变原生产物加进程锁，异常路径也必须显式释放。 */
豫言值 豫言_锁原生产物(豫言值 路径值) {
    int 锁 = open(豫言值转字符串(路径值), O_CREAT | O_RDWR | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (锁 < 0) 报错并中止("无法打开原生产物锁");
    if (flock(锁, LOCK_EX) != 0) { close(锁); 报错并中止("无法取得原生产物锁"); }
    return 整数转豫言值(锁);
}

豫言值 豫言_释原生产物锁(豫言值 锁值) {
    close((int)豫言值转整数(锁值));
    return 单元转豫言值();
}
