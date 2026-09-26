#define _GNU_SOURCE
#include "公共包含.h"
#include "信号名.h"

#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#if defined(__APPLE__)
#include <util.h>
#else
#include <pty.h>
#endif

extern char **environ;

static 豫言值 伪终端整数三项(int64_t 状态, int64_t 甲, int64_t 乙) {
    豫言值 项[] = {整数转豫言值(状态), 整数转豫言值(甲), 整数转豫言值(乙)};
    return 元组转豫言值(3, 项);
}

static 豫言值 伪终端尺寸五项(int64_t 状态, int64_t 列, int64_t 行, int64_t 像列, int64_t 像行) {
    豫言值 项[] = {整数转豫言值(状态), 整数转豫言值(列), 整数转豫言值(行),
                 整数转豫言值(像列), 整数转豫言值(像行)};
    return 元组转豫言值(5, 项);
}

static int 置描述符标记(int 描述符) {
    int 文件标记 = fcntl(描述符, F_GETFD);
    if (文件标记 < 0 || fcntl(描述符, F_SETFD, 文件标记 | FD_CLOEXEC) < 0) return -1;
    int 状态标记 = fcntl(描述符, F_GETFL);
    if (状态标记 < 0 || fcntl(描述符, F_SETFL, 状态标记 | O_NONBLOCK) < 0) return -1;
    return 0;
}

/* 文言：开二端若败则俱闭，勿遗描述符。汉语：原生 PTY 初始化失败时关闭已取得的两个描述符。 */
豫言值 豫言_伪终端_开启(豫言值 未用) {
    (void)未用;
    int 主端 = -1, 从端 = -1;
    if (openpty(&主端, &从端, NULL, NULL, NULL) < 0) {
        int 错误 = errno;
        if (主端 >= 0) close(主端);
        if (从端 >= 0) close(从端);
        return 伪终端整数三项(-错误, -1, -1);
    }
    if (置描述符标记(主端) < 0 || 置描述符标记(从端) < 0) {
        int 错误 = errno;
        close(主端);
        close(从端);
        return 伪终端整数三项(-错误, -1, -1);
    }
    return 伪终端整数三项(0, 主端, 从端);
}

/* 文言：winsize 各量仅容十六位。汉语：超出宿主窗口尺寸字段范围时返回 EINVAL。 */
豫言值 豫言_伪终端_设置尺寸(豫言值 描述符值, 豫言值 列值, 豫言值 行值,
                            豫言值 像列值, 豫言值 像行值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    int64_t 列 = 豫言值转整数(列值), 行 = 豫言值转整数(行值);
    int64_t 像列 = 豫言值转整数(像列值), 像行 = 豫言值转整数(像行值);
    if (描述符 < 0 || 描述符 > INT_MAX || 列 < 0 || 列 > UINT16_MAX ||
        行 < 0 || 行 > UINT16_MAX || 像列 < 0 || 像列 > UINT16_MAX ||
        像行 < 0 || 像行 > UINT16_MAX) return 整数转豫言值(-EINVAL);
    struct winsize 尺寸 = {(unsigned short)行, (unsigned short)列,
                           (unsigned short)像列, (unsigned short)像行};
    if (ioctl((int)描述符, TIOCSWINSZ, &尺寸) < 0) return 整数转豫言值(-errno);
    return 整数转豫言值(0);
}

豫言值 豫言_伪终端_读取尺寸(豫言值 描述符值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    if (描述符 < 0 || 描述符 > INT_MAX) return 伪终端尺寸五项(-EINVAL, 0, 0, 0, 0);
    struct winsize 尺寸;
    if (ioctl((int)描述符, TIOCGWINSZ, &尺寸) < 0) return 伪终端尺寸五项(-errno, 0, 0, 0, 0);
    return 伪终端尺寸五项(0, 尺寸.ws_col, 尺寸.ws_row, 尺寸.ws_xpixel, 尺寸.ws_ypixel);
}

/* 文言：从端可置原始模式，字节不受规范行缓冲或回显吞弃。汉语：将指定 PTY 端设为 termios raw 模式；调用方负责决定是否符合客户端请求。 */
豫言值 豫言_伪终端_设置原始模式(豫言值 描述符值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    if (描述符 < 0 || 描述符 > INT_MAX) return 整数转豫言值(-EINVAL);
    struct termios 属性;
    if (tcgetattr((int)描述符, &属性) < 0) return 整数转豫言值(-errno);
    cfmakeraw(&属性);
    if (tcsetattr((int)描述符, TCSANOW, &属性) < 0) return 整数转豫言值(-errno);
    return 整数转豫言值(0);
}

/* 文言：禁回显而存规范行与 EOF 之义。汉语：关闭 PTY 回显，保留规范行处理，使 EOT 仍可表示输入结束。 */
豫言值 豫言_伪终端_关闭回显(豫言值 描述符值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    if (描述符 < 0 || 描述符 > INT_MAX) return 整数转豫言值(-EINVAL);
    struct termios 属性;
    if (tcgetattr((int)描述符, &属性) < 0) return 整数转豫言值(-errno);
    属性.c_lflag &= ~ECHO;
#if defined(ECHONL)
    属性.c_lflag &= ~ECHONL;
#endif
    if (tcsetattr((int)描述符, TCSANOW, &属性) < 0) return 整数转豫言值(-errno);
    return 整数转豫言值(0);
}

/* 文言：读者负误码、零终流、正已读；汉语：PTY 读取为非阻塞，最多一次分配 64 KiB。 */
豫言值 豫言_伪终端_读取(豫言值 描述符值, 豫言值 最大数值) {
    int64_t 描述符 = 豫言值转整数(描述符值), 最大数 = 豫言值转整数(最大数值);
    if (描述符 < 0 || 描述符 > INT_MAX || 最大数 < 1 || 最大数 > 65536) {
        豫言值 项[] = {整数转豫言值(-EINVAL), 复制字节为豫言值(0, "")};
        return 元组转豫言值(2, 项);
    }
    unsigned char *缓冲 = malloc((size_t)最大数);
    if (缓冲 == NULL) {
        豫言值 项[] = {整数转豫言值(-ENOMEM), 复制字节为豫言值(0, "")};
        return 元组转豫言值(2, 项);
    }
    ssize_t 长度;
    do { 长度 = read((int)描述符, 缓冲, (size_t)最大数); } while (长度 < 0 && errno == EINTR);
    /* 文言：彼端既闭，Linux 主端或报 EIO；同作 EOF。汉语：将 PTY 对端关闭时的 EIO 规范化为零长度 EOF。 */
    if (长度 < 0 && errno == EIO) 长度 = 0;
    int 错误 = 长度 < 0 ? errno : 0;
    豫言值 项[] = {整数转豫言值(长度 < 0 ? -错误 : 长度),
                 复制字节为豫言值(长度 > 0 ? (size_t)长度 : 0, 缓冲)};
    free(缓冲);
    return 元组转豫言值(2, 项);
}

豫言值 豫言_伪终端_写入(豫言值 描述符值, 豫言值 内容) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    if (描述符 < 0 || 描述符 > INT_MAX) return 整数转豫言值(-EINVAL);
    uint64_t 长度 = 获取豫言_字节串长度(内容);
    if (长度 > SSIZE_MAX) return 整数转豫言值(-EINVAL);
    ssize_t 已写;
    do { 已写 = write((int)描述符, 豫言值转字节串指针(内容), (size_t)长度); }
    while (已写 < 0 && errno == EINTR);
    return 整数转豫言值(已写 < 0 ? -errno : 已写);
}

豫言值 豫言_伪终端_关闭(豫言值 描述符值) {
    int64_t 描述符 = 豫言值转整数(描述符值);
    if (描述符 < 0 || 描述符 > INT_MAX) return 整数转豫言值(-EINVAL);
    if (close((int)描述符) < 0) return 整数转豫言值(-errno);
    return 整数转豫言值(0);
}

/* 文言：候主端可读，零为逾时，一为有事，负为误。汉语：等待 PTY 数据或挂断，可用于非阻塞会话循环。 */
豫言值 豫言_伪终端_等待可读(豫言值 描述符值, 豫言值 毫秒值) {
    int64_t 描述符 = 豫言值转整数(描述符值), 毫秒 = 豫言值转整数(毫秒值);
    if (描述符 < 0 || 描述符 > INT_MAX || 毫秒 < 0 || 毫秒 > 60000) return 整数转豫言值(-EINVAL);
    struct pollfd 项 = {.fd = (int)描述符, .events = POLLIN | POLLHUP | POLLERR};
    int 结果;
    do { 结果 = poll(&项, 1, (int)毫秒); } while (结果 < 0 && errno == EINTR);
    if (结果 < 0) return 整数转豫言值(-errno);
    return 整数转豫言值(结果 == 0 ? 0 : 1);
}

/* 文言：候主端可写，挂断与误不得充可写。汉语：等待 PTY 可写，超时返回 0，只有 POLLOUT 返回 1。 */
豫言值 豫言_伪终端_等待可写(豫言值 描述符值, 豫言值 毫秒值) {
    int64_t 描述符 = 豫言值转整数(描述符值), 毫秒 = 豫言值转整数(毫秒值);
    if (描述符 < 0 || 描述符 > INT_MAX || 毫秒 < 0 || 毫秒 > 60000) return 整数转豫言值(-EINVAL);
    struct pollfd 项 = {.fd = (int)描述符, .events = POLLOUT};
    int 结果;
    do { 结果 = poll(&项, 1, (int)毫秒); } while (结果 < 0 && errno == EINTR);
    if (结果 < 0) return 整数转豫言值(-errno);
    if (结果 == 0) return 整数转豫言值(0);
    if (项.revents & POLLNVAL) return 整数转豫言值(-EBADF);
    if (项.revents & POLLOUT) return 整数转豫言值(1);
    if (项.revents & (POLLERR | POLLHUP)) return 整数转豫言值(-EPIPE);
    return 整数转豫言值(-EIO);
}

typedef struct 伪终端子进程记录 {
    pid_t 进程号;
    struct 伪终端子进程记录 *下一;
} 伪终端子进程记录;

static pthread_mutex_t 伪终端子进程锁 = PTHREAD_MUTEX_INITIALIZER;
static 伪终端子进程记录 *伪终端子进程们 = NULL;

static 豫言值 伪终端进程二项(int64_t 状态, int64_t 进程号) {
    豫言值 项[] = {整数转豫言值(状态), 整数转豫言值(进程号)};
    return 元组转豫言值(2, 项);
}

static 伪终端子进程记录 **寻伪终端进程(pid_t 进程号) {
    伪终端子进程记录 **位置 = &伪终端子进程们;
    while (*位置 != NULL && (*位置)->进程号 != 进程号) 位置 = &(*位置)->下一;
    return 位置;
}

/* 文言：程序必绝对径，子进程另立会话，从端为三常流；不授远客未核之令。汉语：仅启动调用方明确授权的绝对路径程序，PTY 从端连接标准流并建立新会话。 */
豫言值 豫言_伪终端_启动程序(豫言值 从端值, 豫言值 程序值, 豫言值 参数列) {
    int64_t 从端 = 豫言值转整数(从端值);
    const char *程序 = 豫言值转字符串(程序值);
    if (从端 < 0 || 从端 > INT_MAX || 程序[0] != '/') return 伪终端进程二项(-EINVAL, -1);
#if !defined(POSIX_SPAWN_SETSID)
    return 伪终端进程二项(-ENOTSUP, -1);
#else
    char 终端路径[PATH_MAX];
    int 错误 = ttyname_r((int)从端, 终端路径, sizeof(终端路径));
    if (错误 != 0) return 伪终端进程二项(-错误, -1);

    uint64_t 参数数 = 获取同构列长度(参数列);
    if (参数数 > 1024 || 参数数 > (SIZE_MAX / sizeof(char *)) - 2) {
        return 伪终端进程二项(-E2BIG, -1);
    }
    char **参数 = malloc(sizeof(char *) * ((size_t)参数数 + 2));
    伪终端子进程记录 *记录 = malloc(sizeof(*记录));
    if (参数 == NULL || 记录 == NULL) {
        free(参数);
        free(记录);
        return 伪终端进程二项(-ENOMEM, -1);
    }
    参数[0] = (char *)程序;
    豫言值 *原参数 = 获取同构列元素(参数列);
    for (uint64_t 序 = 0; 序 < 参数数; ++序) 参数[序 + 1] = 豫言值转字符串(原参数[序]);
    参数[参数数 + 1] = NULL;

    posix_spawn_file_actions_t 操作;
    posix_spawnattr_t 属性;
    错误 = posix_spawn_file_actions_init(&操作);
    if (错误 != 0) goto 释放参数;
    错误 = posix_spawnattr_init(&属性);
    if (错误 != 0) goto 释放操作;
    错误 = posix_spawnattr_setflags(&属性, POSIX_SPAWN_SETSID);
    if (错误 == 0) 错误 = posix_spawn_file_actions_addopen(&操作, STDIN_FILENO, 终端路径, O_RDWR, 0);
    if (错误 == 0) 错误 = posix_spawn_file_actions_adddup2(&操作, STDIN_FILENO, STDOUT_FILENO);
    if (错误 == 0) 错误 = posix_spawn_file_actions_adddup2(&操作, STDIN_FILENO, STDERR_FILENO);
    pid_t 进程号 = -1;
    if (错误 == 0) 错误 = posix_spawn(&进程号, 程序, &操作, &属性, 参数, environ);
    posix_spawnattr_destroy(&属性);
释放操作:
    posix_spawn_file_actions_destroy(&操作);
释放参数:
    free(参数);
    if (错误 != 0) {
        free(记录);
        return 伪终端进程二项(-错误, -1);
    }
    记录->进程号 = 进程号;
    pthread_mutex_lock(&伪终端子进程锁);
    记录->下一 = 伪终端子进程们;
    伪终端子进程们 = 记录;
    pthread_mutex_unlock(&伪终端子进程锁);
    return 伪终端进程二项(0, 进程号);
#endif
}

/* 文言：惟收自开之子进程，未终返零，终则摘籍。汉语：仅轮询本库启动的子进程，终止后从登记表移除并回收。 */
豫言值 豫言_伪终端_收取程序(豫言值 进程号值) {
    int64_t 原进程号 = 豫言值转整数(进程号值);
    豫言值 项[4];
    if (原进程号 < 1 || 原进程号 > INT_MAX) {
        项[0] = 整数转豫言值(-EINVAL);
        项[1] = 整数转豫言值(0);
        项[2] = 整数转豫言值(0);
        项[3] = 整数转豫言值(0);
        return 元组转豫言值(4, 项);
    }
    pid_t 进程号 = (pid_t)原进程号;
    pthread_mutex_lock(&伪终端子进程锁);
    伪终端子进程记录 **位置 = 寻伪终端进程(进程号);
    if (*位置 == NULL) {
        pthread_mutex_unlock(&伪终端子进程锁);
        项[0] = 整数转豫言值(-ECHILD);
        项[1] = 整数转豫言值(0);
        项[2] = 整数转豫言值(0);
        项[3] = 整数转豫言值(0);
        return 元组转豫言值(4, 项);
    }
    int 状态 = 0;
    pid_t 已收;
    do { 已收 = waitpid(进程号, &状态, WNOHANG); } while (已收 < 0 && errno == EINTR);
    if (已收 == 进程号 || (已收 < 0 && errno == ECHILD)) {
        伪终端子进程记录 *旧 = *位置;
        *位置 = 旧->下一;
        free(旧);
    }
    int 错误 = 已收 < 0 ? errno : 0;
    pthread_mutex_unlock(&伪终端子进程锁);
    int64_t 种 = 已收 < 0 ? -错误 : (已收 == 0 ? 0 : (WIFEXITED(状态) ? 1 : 2));
    项[0] = 整数转豫言值(种);
    项[1] = 整数转豫言值(已收 > 0 && WIFEXITED(状态) ? WEXITSTATUS(状态) : 0);
    项[2] = 整数转豫言值(已收 > 0 && WIFSIGNALED(状态) ? WTERMSIG(状态) : 0);
#if defined(WCOREDUMP)
    项[3] = 整数转豫言值(已收 > 0 && WIFSIGNALED(状态) && WCOREDUMP(状态) ? 1 : 0);
#else
    项[3] = 整数转豫言值(0);
#endif
    return 元组转豫言值(4, 项);
}

/* 文言：惟向登记在籍之会话进程组发 TERM。汉语：只向尚登记的 PTY 子进程组发送 SIGTERM，避免误杀无关 PID。 */
豫言值 豫言_伪终端_终止程序(豫言值 进程号值) {
    int64_t 原进程号 = 豫言值转整数(进程号值);
    if (原进程号 < 1 || 原进程号 > INT_MAX) return 整数转豫言值(-EINVAL);
    pid_t 进程号 = (pid_t)原进程号;
    pthread_mutex_lock(&伪终端子进程锁);
    if (*寻伪终端进程(进程号) == NULL) {
        pthread_mutex_unlock(&伪终端子进程锁);
        return 整数转豫言值(-ECHILD);
    }
    int 结果 = kill(-进程号, SIGTERM);
    int 错误 = 结果 < 0 ? errno : 0;
    pthread_mutex_unlock(&伪终端子进程锁);
    return 整数转豫言值(结果 < 0 ? -错误 : 0);
}

/* 文言：惟在籍会话进程组可受 RFC 4254 常名之信号。汉语：仅向登记的 PTY 会话进程组发送标准信号。 */
豫言值 豫言_伪终端_发送信号(豫言值 进程号值, 豫言值 名称值) {
    int64_t 原号 = 豫言值转整数(进程号值);
    uint64_t 长度 = 获取豫言_字节串长度(名称值);
    if (原号 < 1 || 原号 > INT_MAX || 长度 == 0 || 长度 > 64)
        return 整数转豫言值(-EINVAL);
    int 编号 = 安全外壳信号编号(豫言值转字节串指针(名称值), (size_t)长度);
    if (编号 == 0) return 整数转豫言值(-EINVAL);
    pid_t 进程号 = (pid_t)原号;
    pthread_mutex_lock(&伪终端子进程锁);
    if (*寻伪终端进程(进程号) == NULL) {
        pthread_mutex_unlock(&伪终端子进程锁);
        return 整数转豫言值(-ECHILD);
    }
    int 结果 = kill(-进程号, 编号);
    int 错误 = 结果 < 0 ? errno : 0;
    pthread_mutex_unlock(&伪终端子进程锁);
    return 整数转豫言值(结果 < 0 ? -错误 : 0);
}

/* 文言：宿主号返 RFC 名，未列者返空。汉语：把平台信号号转换为 RFC 4254 标准信号名。 */
豫言值 豫言_伪终端_信号名称(豫言值 编号值) {
    int64_t 原号 = 豫言值转整数(编号值);
    if (原号 < 1 || 原号 > INT_MAX) return 复制字节为豫言值(0, "");
    const char *名称 = 安全外壳信号名称((int)原号);
    return 名称 == NULL ? 复制字节为豫言值(0, "") : 复制字节为豫言值(strlen(名称), 名称);
}
