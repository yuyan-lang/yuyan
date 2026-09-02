#include "传输控制协议.h"

#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <sys/socket.h>


enum {
    传输控制协议状态成功 = 0,
    传输控制协议状态等待 = 1,
    传输控制协议状态已关闭 = 2,
};

enum {
    传输控制协议关注可读 = 1,
    传输控制协议关注可写 = 2,
};

enum {
    传输控制协议事件可读 = 1,
    传输控制协议事件可写 = 2,
    传输控制协议事件错误 = 4,
    传输控制协议事件挂断 = 8,
};

/* getaddrinfo 的错误码与 errno 不是同一编号空间。 */
static const int64_t 传输控制协议地址解析错误基数 = -20000;


static 豫言值 构造传输控制协议结果(int64_t 状态, 豫言值 值) {
    豫言值 元素[] = {
        整数转豫言值(状态),
        值,
    };
    return 元组转豫言值(2, 元素);
}

static 豫言值 构造传输控制协议整数结果(int64_t 状态, int64_t 值) {
    return 构造传输控制协议结果(状态, 整数转豫言值(值));
}

static 豫言值 构造传输控制协议单元结果(int64_t 状态) {
    return 构造传输控制协议结果(状态, 单元转豫言值());
}

static int64_t 传输控制协议错误状态(int 错误码) {
    return -(int64_t)错误码;
}

static int64_t 传输控制协议地址解析错误状态(int 错误码) {
    return 传输控制协议地址解析错误基数 + 错误码;
}

static bool 是等待错误(int 错误码) {
    return 错误码 == EAGAIN || 错误码 == EWOULDBLOCK;
}

static int 设置套接字非阻塞且执行时关闭(int 套接字) {
    int 状态标志 = fcntl(套接字, F_GETFL, 0);
    if (状态标志 < 0) {
        return -1;
    }
    if (fcntl(套接字, F_SETFL, 状态标志 | O_NONBLOCK) < 0) {
        return -1;
    }

    int 描述符标志 = fcntl(套接字, F_GETFD, 0);
    if (描述符标志 < 0) {
        return -1;
    }
    if (fcntl(套接字, F_SETFD, 描述符标志 | FD_CLOEXEC) < 0) {
        return -1;
    }

#if defined(SO_NOSIGPIPE)
    int 启用 = 1;
    if (setsockopt(套接字, SOL_SOCKET, SO_NOSIGPIPE, &启用, sizeof(启用)) < 0) {
        return -1;
    }
#endif

    return 0;
}

static int 创建非阻塞套接字(int 地址族, int 类型, int 协议) {
    int 套接字 = socket(地址族, 类型, 协议);
    if (套接字 < 0) {
        return -1;
    }
    if (设置套接字非阻塞且执行时关闭(套接字) < 0) {
        int 保存错误码 = errno;
        close(套接字);
        errno = 保存错误码;
        return -1;
    }
    return 套接字;
}

static int 检查端口(豫言值 端口参数, char 端口文本[6]) {
    int64_t 端口 = 豫言值转整数(端口参数);
    if (端口 < 0 || 端口 > 65535) {
        errno = EINVAL;
        return -1;
    }
    snprintf(端口文本, 6, "%" PRId64, 端口);
    return 0;
}

static 豫言值 复制字节为豫言字符串(const char *字节, size_t 长度) {
    豫言值 字符串 = 分配豫言_字符串缓冲区(长度 + 1);
    char *目标 = 豫言值转堆字符串指针(字符串);
    memcpy(目标, 字节, 长度);
    目标[长度] = '\0';
    return 字符串;
}


豫言值 豫言_传输控制协议_监听(豫言值 地址参数, 豫言值 端口参数, 豫言值 等待队列长度参数) {
    const char *地址 = 豫言值转字符串(地址参数);
    int64_t 等待队列长度 = 豫言值转整数(等待队列长度参数);
    char 端口文本[6];

    if (等待队列长度 < 0 || 等待队列长度 > INT_MAX || 检查端口(端口参数, 端口文本) < 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(EINVAL), -1);
    }

    struct addrinfo 提示;
    memset(&提示, 0, sizeof(提示));
    提示.ai_family = AF_UNSPEC;
    提示.ai_socktype = SOCK_STREAM;
    提示.ai_protocol = IPPROTO_TCP;
    提示.ai_flags = AI_PASSIVE;

    struct addrinfo *地址们 = NULL;
    int 解析结果 = getaddrinfo(地址[0] == '\0' ? NULL : 地址, 端口文本, &提示, &地址们);
    if (解析结果 != 0) {
        return 构造传输控制协议整数结果(传输控制协议地址解析错误状态(解析结果), -1);
    }

    int 最后错误码 = EADDRNOTAVAIL;
    int 监听套接字 = -1;
    for (struct addrinfo *当前地址 = 地址们; 当前地址 != NULL; 当前地址 = 当前地址->ai_next) {
        监听套接字 = 创建非阻塞套接字(
            当前地址->ai_family,
            当前地址->ai_socktype,
            当前地址->ai_protocol
        );
        if (监听套接字 < 0) {
            最后错误码 = errno;
            continue;
        }

        int 启用 = 1;
        if (setsockopt(监听套接字, SOL_SOCKET, SO_REUSEADDR, &启用, sizeof(启用)) < 0) {
            最后错误码 = errno;
            close(监听套接字);
            监听套接字 = -1;
            continue;
        }

        if (bind(监听套接字, 当前地址->ai_addr, 当前地址->ai_addrlen) < 0) {
            最后错误码 = errno;
            close(监听套接字);
            监听套接字 = -1;
            continue;
        }

        if (listen(监听套接字, (int)等待队列长度) < 0) {
            最后错误码 = errno;
            close(监听套接字);
            监听套接字 = -1;
            continue;
        }

        break;
    }

    freeaddrinfo(地址们);
    if (监听套接字 < 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(最后错误码), -1);
    }
    return 构造传输控制协议整数结果(传输控制协议状态成功, 监听套接字);
}

豫言值 豫言_传输控制协议_开始连接(豫言值 主机参数, 豫言值 端口参数) {
    const char *主机 = 豫言值转字符串(主机参数);
    char 端口文本[6];
    if (主机[0] == '\0' || 检查端口(端口参数, 端口文本) < 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(EINVAL), -1);
    }

    struct addrinfo 提示;
    memset(&提示, 0, sizeof(提示));
    提示.ai_family = AF_UNSPEC;
    提示.ai_socktype = SOCK_STREAM;
    提示.ai_protocol = IPPROTO_TCP;

    struct addrinfo *地址们 = NULL;
    int 解析结果 = getaddrinfo(主机, 端口文本, &提示, &地址们);
    if (解析结果 != 0) {
        return 构造传输控制协议整数结果(传输控制协议地址解析错误状态(解析结果), -1);
    }

    int 最后错误码 = ECONNREFUSED;
    for (struct addrinfo *当前地址 = 地址们; 当前地址 != NULL; 当前地址 = 当前地址->ai_next) {
        int 套接字 = 创建非阻塞套接字(
            当前地址->ai_family,
            当前地址->ai_socktype,
            当前地址->ai_protocol
        );
        if (套接字 < 0) {
            最后错误码 = errno;
            continue;
        }

        if (connect(套接字, 当前地址->ai_addr, 当前地址->ai_addrlen) == 0) {
            freeaddrinfo(地址们);
            return 构造传输控制协议整数结果(传输控制协议状态成功, 套接字);
        }

        int 连接错误码 = errno;
        if (连接错误码 == EINPROGRESS || 连接错误码 == EALREADY || 连接错误码 == EINTR) {
            freeaddrinfo(地址们);
            return 构造传输控制协议整数结果(传输控制协议状态等待, 套接字);
        }

        最后错误码 = 连接错误码;
        close(套接字);
    }

    freeaddrinfo(地址们);
    return 构造传输控制协议整数结果(传输控制协议错误状态(最后错误码), -1);
}

豫言值 豫言_传输控制协议_完成连接(豫言值 套接字参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    int 错误码 = 0;
    socklen_t 长度 = sizeof(错误码);
    if (getsockopt(套接字, SOL_SOCKET, SO_ERROR, &错误码, &长度) < 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(errno), 套接字);
    }
    if (错误码 == 0) {
        return 构造传输控制协议整数结果(传输控制协议状态成功, 套接字);
    }
    if (错误码 == EINPROGRESS || 错误码 == EALREADY) {
        return 构造传输控制协议整数结果(传输控制协议状态等待, 套接字);
    }
    return 构造传输控制协议整数结果(传输控制协议错误状态(错误码), 套接字);
}

豫言值 豫言_传输控制协议_接受(豫言值 监听套接字参数) {
    int 监听套接字 = (int)豫言值转整数(监听套接字参数);
    int 套接字;
    do {
        套接字 = accept(监听套接字, NULL, NULL);
    } while (套接字 < 0 && errno == EINTR);

    if (套接字 < 0) {
        if (是等待错误(errno)) {
            return 构造传输控制协议整数结果(传输控制协议状态等待, -1);
        }
        return 构造传输控制协议整数结果(传输控制协议错误状态(errno), -1);
    }

    if (设置套接字非阻塞且执行时关闭(套接字) < 0) {
        int 保存错误码 = errno;
        close(套接字);
        return 构造传输控制协议整数结果(传输控制协议错误状态(保存错误码), -1);
    }

    return 构造传输控制协议整数结果(传输控制协议状态成功, 套接字);
}

豫言值 豫言_传输控制协议_读取(豫言值 套接字参数, 豫言值 最大字节数参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    int64_t 最大字节数 = 豫言值转整数(最大字节数参数);
    if (最大字节数 < 0 || 最大字节数 > UINT32_MAX - 1) {
        return 构造传输控制协议结果(传输控制协议错误状态(EINVAL), 静态字符串转豫言值(""));
    }
    if (最大字节数 == 0) {
        return 构造传输控制协议结果(传输控制协议状态成功, 静态字符串转豫言值(""));
    }

    char *缓冲区 = malloc((size_t)最大字节数);
    if (缓冲区 == NULL) {
        return 构造传输控制协议结果(传输控制协议错误状态(ENOMEM), 静态字符串转豫言值(""));
    }

    ssize_t 读取字节数;
    do {
        读取字节数 = recv(套接字, 缓冲区, (size_t)最大字节数, 0);
    } while (读取字节数 < 0 && errno == EINTR);

    if (读取字节数 > 0) {
        豫言值 内容 = 复制字节为豫言字符串(缓冲区, (size_t)读取字节数);
        free(缓冲区);
        return 构造传输控制协议结果(传输控制协议状态成功, 内容);
    }

    free(缓冲区);
    if (读取字节数 == 0) {
        return 构造传输控制协议结果(传输控制协议状态已关闭, 静态字符串转豫言值(""));
    }
    if (是等待错误(errno)) {
        return 构造传输控制协议结果(传输控制协议状态等待, 静态字符串转豫言值(""));
    }
    return 构造传输控制协议结果(传输控制协议错误状态(errno), 静态字符串转豫言值(""));
}

豫言值 豫言_传输控制协议_从字节序数写入(
    豫言值 套接字参数,
    豫言值 内容参数,
    豫言值 起始字节序数参数
) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    const char *内容 = 豫言值转字符串(内容参数);
    uint64_t 内容长度 = 获取豫言_字符串长度(内容参数);
    int64_t 起始字节序数 = 豫言值转整数(起始字节序数参数);
    if (起始字节序数 < 0 || (uint64_t)起始字节序数 > 内容长度) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(EINVAL), 0);
    }
    if ((uint64_t)起始字节序数 == 内容长度) {
        return 构造传输控制协议整数结果(传输控制协议状态成功, 0);
    }

    int 发送标志 = 0;
#if defined(MSG_NOSIGNAL)
    发送标志 |= MSG_NOSIGNAL;
#endif

    ssize_t 写入字节数;
    do {
        写入字节数 = send(
            套接字,
            内容 + 起始字节序数,
            内容长度 - (uint64_t)起始字节序数,
            发送标志
        );
    } while (写入字节数 < 0 && errno == EINTR);

    if (写入字节数 >= 0) {
        return 构造传输控制协议整数结果(传输控制协议状态成功, 写入字节数);
    }
    if (是等待错误(errno)) {
        return 构造传输控制协议整数结果(传输控制协议状态等待, 0);
    }
    return 构造传输控制协议整数结果(传输控制协议错误状态(errno), 0);
}

豫言值 豫言_传输控制协议_等待(豫言值 套接字参数, 豫言值 关注事件参数, 豫言值 超时毫秒数参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    int64_t 关注事件 = 豫言值转整数(关注事件参数);
    int64_t 超时毫秒数 = 豫言值转整数(超时毫秒数参数);
    if ((关注事件 & ~(传输控制协议关注可读 | 传输控制协议关注可写)) != 0 || 关注事件 == 0
        || 超时毫秒数 < -1 || 超时毫秒数 > INT_MAX) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(EINVAL), 0);
    }

    struct pollfd 描述符;
    memset(&描述符, 0, sizeof(描述符));
    描述符.fd = 套接字;
    if ((关注事件 & 传输控制协议关注可读) != 0) {
        描述符.events |= POLLIN;
    }
    if ((关注事件 & 传输控制协议关注可写) != 0) {
        描述符.events |= POLLOUT;
    }

    int 结果;
    do {
        结果 = poll(&描述符, 1, (int)超时毫秒数);
    } while (结果 < 0 && errno == EINTR);

    if (结果 < 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(errno), 0);
    }
    if (结果 == 0) {
        return 构造传输控制协议整数结果(传输控制协议状态成功, 0);
    }
    if ((描述符.revents & POLLNVAL) != 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(EBADF), 0);
    }

    int64_t 发生事件 = 0;
    if ((描述符.revents & (POLLIN | POLLPRI)) != 0) {
        发生事件 |= 传输控制协议事件可读;
    }
    if ((描述符.revents & POLLOUT) != 0) {
        发生事件 |= 传输控制协议事件可写;
    }
    if ((描述符.revents & POLLERR) != 0) {
        发生事件 |= 传输控制协议事件错误;
    }
    if ((描述符.revents & POLLHUP) != 0) {
        发生事件 |= 传输控制协议事件挂断;
    }

    return 构造传输控制协议整数结果(传输控制协议状态成功, 发生事件);
}

豫言值 豫言_传输控制协议_获取本地端口(豫言值 套接字参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    struct sockaddr_storage 地址;
    socklen_t 地址长度 = sizeof(地址);
    if (getsockname(套接字, (struct sockaddr *)&地址, &地址长度) < 0) {
        return 构造传输控制协议整数结果(传输控制协议错误状态(errno), -1);
    }

    int64_t 端口;
    if (地址.ss_family == AF_INET) {
        端口 = ntohs(((struct sockaddr_in *)&地址)->sin_port);
    } else if (地址.ss_family == AF_INET6) {
        端口 = ntohs(((struct sockaddr_in6 *)&地址)->sin6_port);
    } else {
        return 构造传输控制协议整数结果(传输控制协议错误状态(EAFNOSUPPORT), -1);
    }
    return 构造传输控制协议整数结果(传输控制协议状态成功, 端口);
}

豫言值 豫言_传输控制协议_设置无延迟(豫言值 套接字参数, 豫言值 是否启用参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    int 是否启用 = 豫言值转爻(是否启用参数) ? 1 : 0;
    if (setsockopt(套接字, IPPROTO_TCP, TCP_NODELAY, &是否启用, sizeof(是否启用)) < 0) {
        return 构造传输控制协议单元结果(传输控制协议错误状态(errno));
    }
    return 构造传输控制协议单元结果(传输控制协议状态成功);
}

豫言值 豫言_传输控制协议_关闭写入(豫言值 套接字参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    if (shutdown(套接字, SHUT_WR) < 0) {
        return 构造传输控制协议单元结果(传输控制协议错误状态(errno));
    }
    return 构造传输控制协议单元结果(传输控制协议状态成功);
}

豫言值 豫言_传输控制协议_关闭(豫言值 套接字参数) {
    int 套接字 = (int)豫言值转整数(套接字参数);
    if (close(套接字) < 0) {
        return 构造传输控制协议单元结果(传输控制协议错误状态(errno));
    }
    return 构造传输控制协议单元结果(传输控制协议状态成功);
}

豫言值 豫言_传输控制协议_错误消息(豫言值 状态参数) {
    int64_t 状态 = 豫言值转整数(状态参数);
    const char *消息;
    if (状态 >= 传输控制协议地址解析错误基数 - 100 && 状态 <= 传输控制协议地址解析错误基数 + 100) {
        消息 = gai_strerror((int)(状态 - 传输控制协议地址解析错误基数));
    } else {
        int64_t 错误码 = 状态 < 0 ? -状态 : 状态;
        if (错误码 > INT_MAX) {
            错误码 = EINVAL;
        }
        消息 = strerror((int)错误码);
    }
    if (消息 == NULL) {
        消息 = "未知传输控制协议错误";
    }
    return 复制字符串为豫言值(strlen(消息) + 1, 消息);
}
