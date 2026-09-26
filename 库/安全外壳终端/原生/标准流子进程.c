#include "公共包含.h"
#include "信号名.h"

#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <sys/socket.h>
#include <sys/wait.h>

extern char **environ;

typedef struct 标准流进程记录 {
    pid_t 进程号;
    struct 标准流进程记录 *下一;
} 标准流进程记录;

static pthread_mutex_t 标准流进程锁 = PTHREAD_MUTEX_INITIALIZER;
static 标准流进程记录 *标准流进程们 = NULL;

static 豫言值 标准流启动结果(int64_t 状态, int64_t 进程号,
                           int64_t 输入, int64_t 输出, int64_t 错误) {
    豫言值 项[] = {整数转豫言值(状态), 整数转豫言值(进程号),
                 整数转豫言值(输入), 整数转豫言值(输出), 整数转豫言值(错误)};
    return 元组转豫言值(5, 项);
}

static 豫言值 标准流读取结果(int64_t 状态, size_t 长度, const void *内容) {
    豫言值 项[] = {整数转豫言值(状态), 复制字节为豫言值(长度, 内容)};
    return 元组转豫言值(2, 项);
}

static 豫言值 标准流退出结果(int64_t 状态, int64_t 退出码, int64_t 信号, int64_t 转储) {
    豫言值 项[] = {整数转豫言值(状态), 整数转豫言值(退出码),
                 整数转豫言值(信号), 整数转豫言值(转储)};
    return 元组转豫言值(4, 项);
}

/* 文言：SSH 命令原字惟去内零且限长，得与宿主字符串同藏。汉语：校验长度和内部 NUL 后，将 SSH command 字节以同一不可变表示交给 execve 风格参数接口。 */
豫言值 豫言_标准流_命令字节转字符串(豫言值 命令) {
    uint64_t 长度 = 获取豫言_字节串长度(命令);
    int 合法 = 长度 > 0 && 长度 <= 65536 &&
        memchr(豫言值转字节串指针(命令), 0, (size_t)长度) == NULL;
    豫言值 项[] = {整数转豫言值(合法 ? 0 : -EINVAL),
                 合法 ? 命令 : 复制字节为豫言值(0, "")};
    return 元组转豫言值(2, 项);
}

static 标准流进程记录 **寻标准流进程(pid_t 进程号) {
    标准流进程记录 **位置 = &标准流进程们;
    while (*位置 != NULL && (*位置)->进程号 != 进程号) 位置 = &(*位置)->下一;
    return 位置;
}

static void 闭二端(int 两端[2]) {
    if (两端[0] >= 0) close(两端[0]);
    if (两端[1] >= 0) close(两端[1]);
    两端[0] = 两端[1] = -1;
}

/* 文言：诸描述符皆移至三以上，且置执行时闭。汉语：避免子进程文件操作误关标准流，并阻止无关进程继承描述符。 */
static int 整理描述符(int *描述符) {
    if (*描述符 < 3) {
        int 新端 = fcntl(*描述符, F_DUPFD, 3);
        if (新端 < 0) return errno;
        close(*描述符);
        *描述符 = 新端;
    }
    int 标记 = fcntl(*描述符, F_GETFD);
    if (标记 < 0 || fcntl(*描述符, F_SETFD, 标记 | FD_CLOEXEC) < 0) return errno;
    return 0;
}

static int 整理二端(int 两端[2]) {
    int 错误 = 整理描述符(&两端[0]);
    if (错误 == 0) 错误 = 整理描述符(&两端[1]);
    return 错误;
}

static int 设非阻塞(int 描述符) {
    int 标记 = fcntl(描述符, F_GETFL);
    if (标记 < 0 || fcntl(描述符, F_SETFL, 标记 | O_NONBLOCK) < 0) return errno;
    return 0;
}

/* 文言：子流别置；输入用无 SIGPIPE 之本地套接，二出各用管道。汉语：以绝对路径启动当前用户程序，返回 PID、可写 stdin 和可读 stdout/stderr 描述符。 */
豫言值 豫言_标准流_启动程序(豫言值 程序值, 豫言值 参数列) {
    const char *程序 = 豫言值转字符串(程序值);
    uint64_t 程序长 = 获取豫言_字符串长度(程序值);
    if (程序长 == 0 || 程序长 >= 4096 || 程序[0] != '/' ||
        strlen(程序) != 程序长) {
        return 标准流启动结果(-EINVAL, -1, -1, -1, -1);
    }
    uint64_t 参数数 = 获取同构列长度(参数列);
    if (参数数 > 1024 || 参数数 > (SIZE_MAX / sizeof(char *)) - 2) {
        return 标准流启动结果(-E2BIG, -1, -1, -1, -1);
    }
    char **参数 = malloc(sizeof(char *) * ((size_t)参数数 + 2));
    标准流进程记录 *记录 = malloc(sizeof(*记录));
    if (参数 == NULL || 记录 == NULL) {
        free(参数);
        free(记录);
        return 标准流启动结果(-ENOMEM, -1, -1, -1, -1);
    }
    参数[0] = (char *)程序;
    豫言值 *原参数 = 获取同构列元素(参数列);
    for (uint64_t 序 = 0; 序 < 参数数; ++序) {
        参数[序 + 1] = 豫言值转字符串(原参数[序]);
        uint64_t 长度 = 获取豫言_字符串长度(原参数[序]);
        if (长度 > 1048576 || strlen(参数[序 + 1]) != 长度) {
            free(参数);
            free(记录);
            return 标准流启动结果(-EINVAL, -1, -1, -1, -1);
        }
    }
    参数[参数数 + 1] = NULL;

    int 输入[2] = {-1, -1}, 输出[2] = {-1, -1}, 错误流[2] = {-1, -1};
    int 错误 = 0;
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, 输入) < 0) 错误 = errno;
    if (错误 == 0 && pipe(输出) < 0) 错误 = errno;
    if (错误 == 0 && pipe(错误流) < 0) 错误 = errno;
    if (错误 == 0) 错误 = 整理二端(输入);
    if (错误 == 0) 错误 = 整理二端(输出);
    if (错误 == 0) 错误 = 整理二端(错误流);
    if (错误 == 0) 错误 = 设非阻塞(输入[1]);
    if (错误 == 0) 错误 = 设非阻塞(输出[0]);
    if (错误 == 0) 错误 = 设非阻塞(错误流[0]);
#if defined(__APPLE__)
    if (错误 == 0) {
        int 禁断信号 = 1;
        if (setsockopt(输入[1], SOL_SOCKET, SO_NOSIGPIPE,
                       &禁断信号, sizeof(禁断信号)) < 0) 错误 = errno;
    }
#endif
    if (错误 != 0) goto 失败;

    posix_spawn_file_actions_t 操作;
    错误 = posix_spawn_file_actions_init(&操作);
    if (错误 != 0) goto 失败;
    错误 = posix_spawn_file_actions_adddup2(&操作, 输入[0], STDIN_FILENO);
    if (错误 == 0) 错误 = posix_spawn_file_actions_adddup2(&操作, 输出[1], STDOUT_FILENO);
    if (错误 == 0) 错误 = posix_spawn_file_actions_adddup2(&操作, 错误流[1], STDERR_FILENO);
    int 六端[] = {输入[0], 输入[1], 输出[0], 输出[1], 错误流[0], 错误流[1]};
    for (int 序 = 0; 错误 == 0 && 序 < 6; ++序) {
        错误 = posix_spawn_file_actions_addclose(&操作, 六端[序]);
    }
    pid_t 进程号 = -1;
    if (错误 == 0) 错误 = posix_spawn(&进程号, 程序, &操作, NULL, 参数, environ);
    posix_spawn_file_actions_destroy(&操作);
    if (错误 != 0) goto 失败;

    close(输入[0]);
    close(输出[1]);
    close(错误流[1]);
    记录->进程号 = 进程号;
    pthread_mutex_lock(&标准流进程锁);
    记录->下一 = 标准流进程们;
    标准流进程们 = 记录;
    pthread_mutex_unlock(&标准流进程锁);
    free(参数);
    return 标准流启动结果(0, 进程号, 输入[1], 输出[0], 错误流[0]);

失败:
    闭二端(输入);
    闭二端(输出);
    闭二端(错误流);
    free(参数);
    free(记录);
    return 标准流启动结果(-错误, -1, -1, -1, -1);
}

豫言值 豫言_标准流_读取(豫言值 描述符值, 豫言值 最大数值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    int64_t 最大数 = 豫言值转整数(最大数值);
    if (描述符 < 0 || 描述符 > INT_MAX || 最大数 < 1 || 最大数 > 65536) {
        return 标准流读取结果(-EINVAL, 0, "");
    }
    unsigned char *缓冲 = malloc((size_t)最大数);
    if (缓冲 == NULL) return 标准流读取结果(-ENOMEM, 0, "");
    ssize_t 长度;
    do { 长度 = read((int)描述符, 缓冲, (size_t)最大数); }
    while (长度 < 0 && errno == EINTR);
    int 错误 = 长度 < 0 ? errno : 0;
    豫言值 结果 = 标准流读取结果(长度 < 0 ? -错误 : 长度,
                                 长度 > 0 ? (size_t)长度 : 0, 缓冲);
    free(缓冲);
    return 结果;
}

豫言值 豫言_标准流_写入(豫言值 描述符值, 豫言值 内容) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    uint64_t 长度 = 获取豫言_字节串长度(内容);
    if (描述符 < 0 || 描述符 > INT_MAX || 长度 > SSIZE_MAX) return 整数转豫言值(-EINVAL);
    ssize_t 已写;
    do {
#if defined(MSG_NOSIGNAL)
        已写 = send((int)描述符, 豫言值转字节串指针(内容), (size_t)长度, MSG_NOSIGNAL);
#else
        已写 = send((int)描述符, 豫言值转字节串指针(内容), (size_t)长度, 0);
#endif
    } while (已写 < 0 && errno == EINTR);
    return 整数转豫言值(已写 < 0 ? -errno : 已写);
}

豫言值 豫言_标准流_关闭(豫言值 描述符值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    if (描述符 < 0 || 描述符 > INT_MAX) return 整数转豫言值(-EINVAL);
    return 整数转豫言值(close((int)描述符) == 0 ? 0 : -errno);
}

/* 文言：候单一端读、写或挂断，零逾时，正为事，负为误。汉语：轮询一个标准流描述符；事件位 1 为可读或挂断，2 为可写。 */
豫言值 豫言_标准流_等待(豫言值 描述符值, 豫言值 事件值, 豫言值 毫秒值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    int64_t 事件 = 豫言值转整数(事件值);
    int64_t 毫秒 = 豫言值转整数(毫秒值);
    if (描述符 < 0 || 描述符 > INT_MAX || 事件 < 1 || 事件 > 3 ||
        毫秒 < 0 || 毫秒 > 60000) return 整数转豫言值(-EINVAL);
    struct pollfd 项 = {.fd = (int)描述符,
                         .events = (short)(((事件 & 1) ? POLLIN : 0) |
                                           ((事件 & 2) ? POLLOUT : 0))};
    int 结果;
    do { 结果 = poll(&项, 1, (int)毫秒); } while (结果 < 0 && errno == EINTR);
    if (结果 < 0) return 整数转豫言值(-errno);
    if (结果 == 0) return 整数转豫言值(0);
    if (项.revents & POLLNVAL) return 整数转豫言值(-EBADF);
    if (项.revents & POLLERR) return 整数转豫言值(-EIO);
    if (项.revents & (POLLIN | POLLHUP | POLLOUT)) return 整数转豫言值(1);
    return 整数转豫言值(0);
}

static int64_t 标准流事件码(short 事件) {
    int64_t 结果 = 0;
    if (事件 & (POLLIN | POLLPRI)) 结果 |= 1;
    if (事件 & POLLOUT) 结果 |= 2;
    if (事件 & (POLLERR | POLLNVAL)) 结果 |= 4;
    if (事件 & POLLHUP) 结果 |= 8;
    return 结果;
}

/* 文言：一候网连与三常流，无关端以负符略之。汉语：同时轮询 SSH 套接字和仍活动的 stdin/stdout/stderr，返回固定位置的事件位。 */
豫言值 豫言_标准流_等待连接与三流(豫言值 连接值, 豫言值 输入值,
                                      豫言值 输出值, 豫言值 误出值,
                                      豫言值 关注值, 豫言值 毫秒值) {
    int64_t 连接 = 豫言值转整数(连接值);
    int64_t 输入 = 豫言值转整数(输入值);
    int64_t 输出 = 豫言值转整数(输出值);
    int64_t 误出 = 豫言值转整数(误出值);
    int64_t 关注 = 豫言值转整数(关注值);
    int64_t 毫秒 = 豫言值转整数(毫秒值);
    if (连接 < 0 || 连接 > INT_MAX || 关注 < 0 || 关注 > 7 ||
        毫秒 < 0 || 毫秒 > 60000 ||
        ((关注 & 1) && (输入 < 0 || 输入 > INT_MAX)) ||
        ((关注 & 2) && (输出 < 0 || 输出 > INT_MAX)) ||
        ((关注 & 4) && (误出 < 0 || 误出 > INT_MAX))) {
        豫言值 项[] = {整数转豫言值(-EINVAL), 整数转豫言值(0),
                     整数转豫言值(0), 整数转豫言值(0), 整数转豫言值(0)};
        return 元组转豫言值(5, 项);
    }
    struct pollfd 项们[4] = {
        {.fd = (int)连接, .events = POLLIN},
        {.fd = (关注 & 1) ? (int)输入 : -1, .events = POLLOUT},
        {.fd = (关注 & 2) ? (int)输出 : -1, .events = POLLIN},
        {.fd = (关注 & 4) ? (int)误出 : -1, .events = POLLIN},
    };
    int 状态;
    do { 状态 = poll(项们, 4, (int)毫秒); } while (状态 < 0 && errno == EINTR);
    豫言值 项[] = {
        整数转豫言值(状态 < 0 ? -errno : 0),
        整数转豫言值(状态 < 0 ? 0 : 标准流事件码(项们[0].revents)),
        整数转豫言值(状态 < 0 ? 0 : 标准流事件码(项们[1].revents)),
        整数转豫言值(状态 < 0 ? 0 : 标准流事件码(项们[2].revents)),
        整数转豫言值(状态 < 0 ? 0 : 标准流事件码(项们[3].revents)),
    };
    return 元组转豫言值(5, 项);
}

豫言值 豫言_标准流_收取程序(豫言值 进程号值) {
    int64_t 原号 = 豫言值转整数(进程号值);
    if (原号 < 1 || 原号 > INT_MAX) return 标准流退出结果(-EINVAL, 0, 0, 0);
    pid_t 进程号 = (pid_t)原号;
    pthread_mutex_lock(&标准流进程锁);
    标准流进程记录 **位置 = 寻标准流进程(进程号);
    if (*位置 == NULL) {
        pthread_mutex_unlock(&标准流进程锁);
        return 标准流退出结果(-ECHILD, 0, 0, 0);
    }
    int 状态 = 0;
    pid_t 已收;
    do { 已收 = waitpid(进程号, &状态, WNOHANG); } while (已收 < 0 && errno == EINTR);
    if (已收 == 进程号 || (已收 < 0 && errno == ECHILD)) {
        标准流进程记录 *旧 = *位置;
        *位置 = 旧->下一;
        free(旧);
    }
    int 错误 = 已收 < 0 ? errno : 0;
    pthread_mutex_unlock(&标准流进程锁);
    int 转储 = 0;
#if defined(WCOREDUMP)
    if (已收 > 0 && WIFSIGNALED(状态)) 转储 = WCOREDUMP(状态) ? 1 : 0;
#endif
    return 标准流退出结果(已收 < 0 ? -错误 :
                              (已收 == 0 ? 0 : (WIFEXITED(状态) ? 1 : 2)),
                          已收 > 0 && WIFEXITED(状态) ? WEXITSTATUS(状态) : 0,
                          已收 > 0 && WIFSIGNALED(状态) ? WTERMSIG(状态) : 0,
                          转储);
}

豫言值 豫言_标准流_终止程序(豫言值 进程号值) {
    int64_t 原号 = 豫言值转整数(进程号值);
    if (原号 < 1 || 原号 > INT_MAX) return 整数转豫言值(-EINVAL);
    pid_t 进程号 = (pid_t)原号;
    pthread_mutex_lock(&标准流进程锁);
    if (*寻标准流进程(进程号) == NULL) {
        pthread_mutex_unlock(&标准流进程锁);
        return 整数转豫言值(-ECHILD);
    }
    int 结果 = kill(进程号, SIGTERM);
    int 错误 = 结果 < 0 ? errno : 0;
    pthread_mutex_unlock(&标准流进程锁);
    return 整数转豫言值(结果 < 0 ? -错误 : 0);
}

/* 文言：惟在籍 PID 可受 RFC 4254 常名之信号，余名不施。汉语：仅向已登记的子进程发送白名单中的标准信号。 */
豫言值 豫言_标准流_发送信号(豫言值 进程号值, 豫言值 名称值) {
    int64_t 原号 = 豫言值转整数(进程号值);
    uint64_t 长度 = 获取豫言_字节串长度(名称值);
    if (原号 < 1 || 原号 > INT_MAX || 长度 == 0 || 长度 > 64)
        return 整数转豫言值(-EINVAL);
    int 编号 = 安全外壳信号编号(豫言值转字节串指针(名称值), (size_t)长度);
    if (编号 == 0) return 整数转豫言值(-EINVAL);
    pid_t 进程号 = (pid_t)原号;
    pthread_mutex_lock(&标准流进程锁);
    if (*寻标准流进程(进程号) == NULL) {
        pthread_mutex_unlock(&标准流进程锁);
        return 整数转豫言值(-ECHILD);
    }
    int 结果 = kill(进程号, 编号);
    int 错误 = 结果 < 0 ? errno : 0;
    pthread_mutex_unlock(&标准流进程锁);
    return 整数转豫言值(结果 < 0 ? -错误 : 0);
}

/* 文言：宿主号返 RFC 名，未列者返空。汉语：把平台信号号转换为 RFC 4254 标准信号名。 */
豫言值 豫言_标准流_信号名称(豫言值 编号值) {
    int64_t 原号 = 豫言值转整数(编号值);
    if (原号 < 1 || 原号 > INT_MAX) return 复制字节为豫言值(0, "");
    const char *名称 = 安全外壳信号名称((int)原号);
    return 名称 == NULL ? 复制字节为豫言值(0, "") : 复制字节为豫言值(strlen(名称), 名称);
}
