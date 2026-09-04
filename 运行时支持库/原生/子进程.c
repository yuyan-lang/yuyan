#include "公共包含.h"

#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <sys/wait.h>

extern char **environ;

typedef enum {
    子进程标准流忽略,
    子进程标准流捕获,
    子进程标准流继承输出,
    子进程标准流全部继承,
} 子进程标准流模式类型;

typedef struct {
    char *数据;
    size_t 长度;
    size_t 容量;
} 输出缓冲区类型;

static int 初始化输出缓冲区(输出缓冲区类型 *缓冲区) {
    缓冲区->数据 = NULL;
    缓冲区->长度 = 0;
    缓冲区->容量 = 0;
    缓冲区->数据 = malloc(1);
    if (缓冲区->数据 == NULL) {
        return ENOMEM;
    }
    缓冲区->数据[0] = '\0';
    缓冲区->长度 = 0;
    缓冲区->容量 = 1;
    return 0;
}

static void 销毁输出缓冲区(输出缓冲区类型 *缓冲区) {
    free(缓冲区->数据);
    缓冲区->数据 = NULL;
    缓冲区->长度 = 0;
    缓冲区->容量 = 0;
}

static int 追加输出缓冲区(输出缓冲区类型 *缓冲区, const char *数据, size_t 长度) {
    if (长度 > SIZE_MAX - 缓冲区->长度 - 1) {
        return ENOMEM;
    }

    size_t 所需容量 = 缓冲区->长度 + 长度 + 1;
    if (所需容量 > 缓冲区->容量) {
        size_t 新容量 = 缓冲区->容量;
        while (新容量 < 所需容量) {
            if (新容量 > SIZE_MAX / 2) {
                新容量 = 所需容量;
                break;
            }
            新容量 *= 2;
        }

        char *新数据 = realloc(缓冲区->数据, 新容量);
        if (新数据 == NULL) {
            return ENOMEM;
        }
        缓冲区->数据 = 新数据;
        缓冲区->容量 = 新容量;
    }

    memcpy(缓冲区->数据 + 缓冲区->长度, 数据, 长度);
    缓冲区->长度 += 长度;
    缓冲区->数据[缓冲区->长度] = '\0';
    return 0;
}

static int 设置文件描述符标记(int fd, int 命令, int 标记) {
    int 当前项 = fcntl(fd, 命令);
    if (当前项 < 0) {
        return errno;
    }

    int 设置命令 = 命令 == F_GETFD ? F_SETFD : F_SETFL;
    if (fcntl(fd, 设置命令, 当前项 | 标记) < 0) {
        return errno;
    }
    return 0;
}

static int 创建捕获管道(int 管道描述符[2]) {
    if (pipe(管道描述符) < 0) {
        return errno;
    }

    int 结果 = 设置文件描述符标记(管道描述符[0], F_GETFD, FD_CLOEXEC);
    if (结果 == 0) {
        结果 = 设置文件描述符标记(管道描述符[1], F_GETFD, FD_CLOEXEC);
    }
    if (结果 == 0) {
        结果 = 设置文件描述符标记(管道描述符[0], F_GETFL, O_NONBLOCK);
    }
    if (结果 != 0) {
        close(管道描述符[0]);
        close(管道描述符[1]);
        管道描述符[0] = -1;
        管道描述符[1] = -1;
    }
    return 结果;
}

static int 添加关闭操作(posix_spawn_file_actions_t *操作, int fd) {
    return posix_spawn_file_actions_addclose(操作, fd);
}

static int 添加空设备操作(
    posix_spawn_file_actions_t *操作,
    int 目标描述符,
    int 打开标记
) {
    return posix_spawn_file_actions_addopen(
        操作,
        目标描述符,
        "/dev/null",
        打开标记,
        0
    );
}

static int 添加管道操作(
    posix_spawn_file_actions_t *操作,
    int 管道描述符[2],
    int 目标描述符
) {
    int 结果 = posix_spawn_file_actions_adddup2(操作, 管道描述符[1], 目标描述符);
    if (结果 == 0) {
        结果 = 添加关闭操作(操作, 管道描述符[0]);
    }
    if (结果 == 0 && 管道描述符[1] != 目标描述符) {
        结果 = 添加关闭操作(操作, 管道描述符[1]);
    }
    return 结果;
}

static int 派生进程(
    const char *程序,
    char *const 参数组[],
    子进程标准流模式类型 标准流模式,
    pid_t *进程编号,
    int *标准输出描述符,
    int *标准错误描述符
) {
    int 标准输出管道[2] = {-1, -1};
    int 标准错误管道[2] = {-1, -1};
    int 结果 = 0;

    *标准输出描述符 = -1;
    *标准错误描述符 = -1;

    if (标准流模式 == 子进程标准流捕获) {
        结果 = 创建捕获管道(标准输出管道);
        if (结果 == 0) {
            结果 = 创建捕获管道(标准错误管道);
        }
        if (结果 != 0) {
            if (标准输出管道[0] >= 0) {
                close(标准输出管道[0]);
                close(标准输出管道[1]);
            }
            return 结果;
        }
    }

    posix_spawn_file_actions_t 操作;
    结果 = posix_spawn_file_actions_init(&操作);
    if (结果 != 0) {
        goto 清理;
    }

    // 非交互式子进程不从父进程偷读输入。
    if (标准流模式 != 子进程标准流全部继承) {
        结果 = 添加空设备操作(&操作, STDIN_FILENO, O_RDONLY);
        if (结果 != 0) {
            goto 销毁操作;
        }
    }

    if (标准流模式 == 子进程标准流捕获) {
        结果 = 添加管道操作(&操作, 标准输出管道, STDOUT_FILENO);
        if (结果 == 0) {
            结果 = 添加管道操作(&操作, 标准错误管道, STDERR_FILENO);
        }
    } else if (标准流模式 == 子进程标准流忽略) {
        结果 = 添加空设备操作(&操作, STDOUT_FILENO, O_WRONLY);
        if (结果 == 0) {
            结果 = 添加空设备操作(&操作, STDERR_FILENO, O_WRONLY);
        }
    }

    if (结果 == 0) {
        结果 = posix_spawnp(进程编号, 程序, &操作, NULL, 参数组, environ);
    }

销毁操作:
    posix_spawn_file_actions_destroy(&操作);

清理:
    if (标准流模式 == 子进程标准流捕获) {
        if (标准输出管道[1] >= 0) {
            close(标准输出管道[1]);
            标准输出管道[1] = -1;
        }
        if (标准错误管道[1] >= 0) {
            close(标准错误管道[1]);
            标准错误管道[1] = -1;
        }

        if (结果 == 0) {
            *标准输出描述符 = 标准输出管道[0];
            *标准错误描述符 = 标准错误管道[0];
        } else {
            close(标准输出管道[0]);
            close(标准错误管道[0]);
        }
    }
    return 结果;
}

static int 读取可用数据(struct pollfd *轮询描述符, 输出缓冲区类型 *缓冲区) {
    char 数据块[16384];

    for (;;) {
        ssize_t 已读字节数 = read(轮询描述符->fd, 数据块, sizeof(数据块));
        if (已读字节数 > 0) {
            int 结果 = 追加输出缓冲区(缓冲区, 数据块, (size_t)已读字节数);
            if (结果 != 0) {
                return 结果;
            }
            continue;
        }
        if (已读字节数 == 0) {
            close(轮询描述符->fd);
            轮询描述符->fd = -1;
            return 0;
        }
        if (errno == EINTR) {
            continue;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;
        }
        return errno;
    }
}

static int 收集进程输出(
    int 标准输出描述符,
    int 标准错误描述符,
    输出缓冲区类型 *标准输出缓冲区,
    输出缓冲区类型 *标准错误缓冲区
) {
    struct pollfd 轮询描述符组[2] = {
        {.fd = 标准输出描述符, .events = POLLIN},
        {.fd = 标准错误描述符, .events = POLLIN},
    };
    输出缓冲区类型 *缓冲区组[2] = {标准输出缓冲区, 标准错误缓冲区};

    while (轮询描述符组[0].fd >= 0 || 轮询描述符组[1].fd >= 0) {
        int 轮询结果;
        do {
            轮询结果 = poll(轮询描述符组, 2, -1);
        } while (轮询结果 < 0 && errno == EINTR);

        if (轮询结果 < 0) {
            int 结果 = errno;
            if (轮询描述符组[0].fd >= 0) close(轮询描述符组[0].fd);
            if (轮询描述符组[1].fd >= 0) close(轮询描述符组[1].fd);
            return 结果;
        }

        for (int 序数 = 0; 序数 < 2; ++序数) {
            if (轮询描述符组[序数].fd < 0 || 轮询描述符组[序数].revents == 0) {
                continue;
            }
            if (轮询描述符组[序数].revents & POLLNVAL) {
                int 结果 = EBADF;
                if (轮询描述符组[0].fd >= 0) close(轮询描述符组[0].fd);
                if (轮询描述符组[1].fd >= 0) close(轮询描述符组[1].fd);
                return 结果;
            }
            if (轮询描述符组[序数].revents & (POLLIN | POLLHUP | POLLERR)) {
                int 结果 = 读取可用数据(&轮询描述符组[序数], 缓冲区组[序数]);
                if (结果 != 0) {
                    if (轮询描述符组[0].fd >= 0) close(轮询描述符组[0].fd);
                    if (轮询描述符组[1].fd >= 0) close(轮询描述符组[1].fd);
                    return 结果;
                }
            }
        }
    }
    return 0;
}

static int 等待进程(pid_t 进程编号, int *退出状态) {
    int 状态;
    pid_t 结果;
    do {
        结果 = waitpid(进程编号, &状态, 0);
    } while (结果 < 0 && errno == EINTR);

    if (结果 < 0) {
        return errno;
    }
    if (WIFEXITED(状态)) {
        *退出状态 = WEXITSTATUS(状态);
    } else if (WIFSIGNALED(状态)) {
        *退出状态 = 128 + WTERMSIG(状态);
    } else {
        *退出状态 = 1;
    }
    return 0;
}

static char **创建参数向量(豫言值 程序, 豫言值 参数组) {
    uint64_t 参数数量 = 获取同构列长度(参数组);
    if (参数数量 > (SIZE_MAX / sizeof(char *)) - 2) {
        报错并中止("子进程参数过多");
    }

    char **结果 = malloc(sizeof(char *) * ((size_t)参数数量 + 2));
    if (结果 == NULL) {
        报错并中止("无法分配子进程参数数组");
    }

    结果[0] = 豫言值转字符串(程序);
    豫言值 *参数数组 = 获取同构列元素(参数组);
    for (uint64_t 序数 = 0; 序数 < 参数数量; ++序数) {
        结果[序数 + 1] = 豫言值转字符串(参数数组[序数]);
    }
    结果[参数数量 + 1] = NULL;
    return 结果;
}

豫言值 豫言_同步运行子进程并获取输出(豫言值 程序, 豫言值 参数组) {
    char *程序名 = 豫言值转字符串(程序);
    char **参数向量 = 创建参数向量(程序, 参数组);
    输出缓冲区类型 标准输出缓冲区 = {0};
    输出缓冲区类型 标准错误缓冲区 = {0};
    int 结果 = 初始化输出缓冲区(&标准输出缓冲区);
    if (结果 == 0) {
        结果 = 初始化输出缓冲区(&标准错误缓冲区);
    }
    if (结果 != 0) {
        free(参数向量);
        销毁输出缓冲区(&标准输出缓冲区);
        销毁输出缓冲区(&标准错误缓冲区);
        报错并中止("无法分配子进程输出缓冲区");
    }

    pid_t 进程编号;
    int 标准输出描述符;
    int 标准错误描述符;
    结果 = 派生进程(
        程序名,
        参数向量,
        子进程标准流捕获,
        &进程编号,
        &标准输出描述符,
        &标准错误描述符
    );
    free(参数向量);
    if (结果 != 0) {
        fprintf(stderr, "派生子进程失败： %s: %s\n", 程序名, strerror(结果));
        销毁输出缓冲区(&标准输出缓冲区);
        销毁输出缓冲区(&标准错误缓冲区);
        报错并中止("派生子进程失败");
    }

    结果 = 收集进程输出(标准输出描述符, 标准错误描述符, &标准输出缓冲区, &标准错误缓冲区);
    int 退出状态 = 1;
    int 等待结果 = 等待进程(进程编号, &退出状态);
    if (结果 == 0) {
        结果 = 等待结果;
    }
    if (结果 != 0) {
        fprintf(stderr, "子进程输入输出失败： %s\n", strerror(结果));
        销毁输出缓冲区(&标准输出缓冲区);
        销毁输出缓冲区(&标准错误缓冲区);
        报错并中止("子进程执行失败");
    }

    豫言值 标准输出值 = 复制字符串为豫言值(
        标准输出缓冲区.长度 + 1,
        标准输出缓冲区.数据
    );
    豫言值 标准错误值 = 复制字符串为豫言值(
        标准错误缓冲区.长度 + 1,
        标准错误缓冲区.数据
    );
    销毁输出缓冲区(&标准输出缓冲区);
    销毁输出缓冲区(&标准错误缓冲区);

    豫言值 值组[] = {
        爻转豫言值(退出状态 == 0),
        标准输出值,
        标准错误值,
    };
    return 元组转豫言值(3, 值组);
}

豫言值 豫言_同步运行子进程(豫言值 程序, 豫言值 参数组) {
    char *程序名 = 豫言值转字符串(程序);
    char **参数向量 = 创建参数向量(程序, 参数组);
    pid_t 进程编号;
    int 未用标准输出描述符;
    int 未用标准错误描述符;
    int 结果 = 派生进程(
        程序名,
        参数向量,
        子进程标准流忽略,
        &进程编号,
        &未用标准输出描述符,
        &未用标准错误描述符
    );
    free(参数向量);
    if (结果 != 0) {
        fprintf(stderr, "派生子进程失败： %s: %s\n", 程序名, strerror(结果));
        return 爻转豫言值(false);
    }

    int 退出状态 = 1;
    结果 = 等待进程(进程编号, &退出状态);
    if (结果 != 0) {
        fprintf(stderr, "等待子进程失败： %s\n", strerror(结果));
        return 爻转豫言值(false);
    }
    return 爻转豫言值(退出状态 == 0);
}

豫言值 豫言_同步运行子进程并传递输出(豫言值 程序, 豫言值 参数组) {
    char *程序名 = 豫言值转字符串(程序);
    char **参数向量 = 创建参数向量(程序, 参数组);
    pid_t 进程编号;
    int 未用标准输出描述符;
    int 未用标准错误描述符;
    int 结果 = 派生进程(
        程序名,
        参数向量,
        子进程标准流继承输出,
        &进程编号,
        &未用标准输出描述符,
        &未用标准错误描述符
    );
    free(参数向量);
    if (结果 != 0) {
        fprintf(stderr, "派生子进程失败： %s: %s\n", 程序名, strerror(结果));
        return 整数转豫言值(结果);
    }

    int 退出状态 = 1;
    结果 = 等待进程(进程编号, &退出状态);
    if (结果 != 0) {
        fprintf(stderr, "等待子进程失败： %s\n", strerror(结果));
        return 整数转豫言值(结果);
    }
    return 整数转豫言值(退出状态);
}

豫言值 豫言_同步运行子进程并继承标准流(豫言值 程序, 豫言值 参数组) {
    char *程序名 = 豫言值转字符串(程序);
    char **参数向量 = 创建参数向量(程序, 参数组);
    pid_t 进程编号;
    int 未用标准输出描述符;
    int 未用标准错误描述符;
    int 结果 = 派生进程(
        程序名,
        参数向量,
        子进程标准流全部继承,
        &进程编号,
        &未用标准输出描述符,
        &未用标准错误描述符
    );
    free(参数向量);
    if (结果 != 0) {
        fprintf(stderr, "派生子进程失败： %s: %s\n", 程序名, strerror(结果));
        return 整数转豫言值(127);
    }

    int 退出状态 = 1;
    结果 = 等待进程(进程编号, &退出状态);
    if (结果 != 0) {
        fprintf(stderr, "等待子进程失败： %s\n", strerror(结果));
        return 整数转豫言值(1);
    }
    return 整数转豫言值(退出状态);
}
