#include "common_include.h"

#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <sys/wait.h>

extern char **environ;

typedef enum {
    CHILD_STDIO_IGNORE,
    CHILD_STDIO_CAPTURE,
    CHILD_STDIO_INHERIT_OUTPUT,
} child_stdio_mode_t;

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} output_buffer_t;

static int output_buffer_init(output_buffer_t *buffer) {
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
    buffer->data = malloc(1);
    if (buffer->data == NULL) {
        return ENOMEM;
    }
    buffer->data[0] = '\0';
    buffer->length = 0;
    buffer->capacity = 1;
    return 0;
}

static void output_buffer_destroy(output_buffer_t *buffer) {
    free(buffer->data);
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static int output_buffer_append(output_buffer_t *buffer, const char *data, size_t length) {
    if (length > SIZE_MAX - buffer->length - 1) {
        return ENOMEM;
    }

    size_t required = buffer->length + length + 1;
    if (required > buffer->capacity) {
        size_t new_capacity = buffer->capacity;
        while (new_capacity < required) {
            if (new_capacity > SIZE_MAX / 2) {
                new_capacity = required;
                break;
            }
            new_capacity *= 2;
        }

        char *new_data = realloc(buffer->data, new_capacity);
        if (new_data == NULL) {
            return ENOMEM;
        }
        buffer->data = new_data;
        buffer->capacity = new_capacity;
    }

    memcpy(buffer->data + buffer->length, data, length);
    buffer->length += length;
    buffer->data[buffer->length] = '\0';
    return 0;
}

static int set_fd_flag(int fd, int command, int flag) {
    int current = fcntl(fd, command);
    if (current < 0) {
        return errno;
    }

    int set_command = command == F_GETFD ? F_SETFD : F_SETFL;
    if (fcntl(fd, set_command, current | flag) < 0) {
        return errno;
    }
    return 0;
}

static int make_capture_pipe(int pipe_fds[2]) {
    if (pipe(pipe_fds) < 0) {
        return errno;
    }

    int result = set_fd_flag(pipe_fds[0], F_GETFD, FD_CLOEXEC);
    if (result == 0) {
        result = set_fd_flag(pipe_fds[1], F_GETFD, FD_CLOEXEC);
    }
    if (result == 0) {
        result = set_fd_flag(pipe_fds[0], F_GETFL, O_NONBLOCK);
    }
    if (result != 0) {
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        pipe_fds[0] = -1;
        pipe_fds[1] = -1;
    }
    return result;
}

static int add_close_action(posix_spawn_file_actions_t *actions, int fd) {
    return posix_spawn_file_actions_addclose(actions, fd);
}

static int add_dev_null_action(
    posix_spawn_file_actions_t *actions,
    int target_fd,
    int open_flags
) {
    return posix_spawn_file_actions_addopen(
        actions,
        target_fd,
        "/dev/null",
        open_flags,
        0
    );
}

static int add_pipe_actions(
    posix_spawn_file_actions_t *actions,
    int pipe_fds[2],
    int target_fd
) {
    int result = posix_spawn_file_actions_adddup2(actions, pipe_fds[1], target_fd);
    if (result == 0) {
        result = add_close_action(actions, pipe_fds[0]);
    }
    if (result == 0 && pipe_fds[1] != target_fd) {
        result = add_close_action(actions, pipe_fds[1]);
    }
    return result;
}

static int spawn_process(
    const char *program,
    char *const arguments[],
    child_stdio_mode_t stdio_mode,
    pid_t *pid,
    int *stdout_fd,
    int *stderr_fd
) {
    int stdout_pipe[2] = {-1, -1};
    int stderr_pipe[2] = {-1, -1};
    int result = 0;

    *stdout_fd = -1;
    *stderr_fd = -1;

    if (stdio_mode == CHILD_STDIO_CAPTURE) {
        result = make_capture_pipe(stdout_pipe);
        if (result == 0) {
            result = make_capture_pipe(stderr_pipe);
        }
        if (result != 0) {
            if (stdout_pipe[0] >= 0) {
                close(stdout_pipe[0]);
                close(stdout_pipe[1]);
            }
            return result;
        }
    }

    posix_spawn_file_actions_t actions;
    result = posix_spawn_file_actions_init(&actions);
    if (result != 0) {
        goto cleanup;
    }

    // Ignored standard streams must still be valid descriptors in the child.
    result = add_dev_null_action(&actions, STDIN_FILENO, O_RDONLY);
    if (result != 0) {
        goto destroy_actions;
    }

    if (stdio_mode == CHILD_STDIO_CAPTURE) {
        result = add_pipe_actions(&actions, stdout_pipe, STDOUT_FILENO);
        if (result == 0) {
            result = add_pipe_actions(&actions, stderr_pipe, STDERR_FILENO);
        }
    } else if (stdio_mode == CHILD_STDIO_IGNORE) {
        result = add_dev_null_action(&actions, STDOUT_FILENO, O_WRONLY);
        if (result == 0) {
            result = add_dev_null_action(&actions, STDERR_FILENO, O_WRONLY);
        }
    }

    if (result == 0) {
        result = posix_spawnp(pid, program, &actions, NULL, arguments, environ);
    }

destroy_actions:
    posix_spawn_file_actions_destroy(&actions);

cleanup:
    if (stdio_mode == CHILD_STDIO_CAPTURE) {
        if (stdout_pipe[1] >= 0) {
            close(stdout_pipe[1]);
            stdout_pipe[1] = -1;
        }
        if (stderr_pipe[1] >= 0) {
            close(stderr_pipe[1]);
            stderr_pipe[1] = -1;
        }

        if (result == 0) {
            *stdout_fd = stdout_pipe[0];
            *stderr_fd = stderr_pipe[0];
        } else {
            close(stdout_pipe[0]);
            close(stderr_pipe[0]);
        }
    }
    return result;
}

static int read_available(struct pollfd *poll_fd, output_buffer_t *buffer) {
    char chunk[16384];

    for (;;) {
        ssize_t bytes_read = read(poll_fd->fd, chunk, sizeof(chunk));
        if (bytes_read > 0) {
            int result = output_buffer_append(buffer, chunk, (size_t)bytes_read);
            if (result != 0) {
                return result;
            }
            continue;
        }
        if (bytes_read == 0) {
            close(poll_fd->fd);
            poll_fd->fd = -1;
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

static int collect_process_output(
    int stdout_fd,
    int stderr_fd,
    output_buffer_t *stdout_buffer,
    output_buffer_t *stderr_buffer
) {
    struct pollfd poll_fds[2] = {
        {.fd = stdout_fd, .events = POLLIN},
        {.fd = stderr_fd, .events = POLLIN},
    };
    output_buffer_t *buffers[2] = {stdout_buffer, stderr_buffer};

    while (poll_fds[0].fd >= 0 || poll_fds[1].fd >= 0) {
        int poll_result;
        do {
            poll_result = poll(poll_fds, 2, -1);
        } while (poll_result < 0 && errno == EINTR);

        if (poll_result < 0) {
            int result = errno;
            if (poll_fds[0].fd >= 0) close(poll_fds[0].fd);
            if (poll_fds[1].fd >= 0) close(poll_fds[1].fd);
            return result;
        }

        for (int i = 0; i < 2; ++i) {
            if (poll_fds[i].fd < 0 || poll_fds[i].revents == 0) {
                continue;
            }
            if (poll_fds[i].revents & POLLNVAL) {
                int result = EBADF;
                if (poll_fds[0].fd >= 0) close(poll_fds[0].fd);
                if (poll_fds[1].fd >= 0) close(poll_fds[1].fd);
                return result;
            }
            if (poll_fds[i].revents & (POLLIN | POLLHUP | POLLERR)) {
                int result = read_available(&poll_fds[i], buffers[i]);
                if (result != 0) {
                    if (poll_fds[0].fd >= 0) close(poll_fds[0].fd);
                    if (poll_fds[1].fd >= 0) close(poll_fds[1].fd);
                    return result;
                }
            }
        }
    }
    return 0;
}

static int wait_for_process(pid_t pid, int *exit_status) {
    int status;
    pid_t result;
    do {
        result = waitpid(pid, &status, 0);
    } while (result < 0 && errno == EINTR);

    if (result < 0) {
        return errno;
    }
    if (WIFEXITED(status)) {
        *exit_status = WEXITSTATUS(status);
    } else if (WIFSIGNALED(status)) {
        *exit_status = 128 + WTERMSIG(status);
    } else {
        *exit_status = 1;
    }
    return 0;
}

static char **make_argument_vector(yyvalue program, yyvalue arguments) {
    uint64_t argument_count = iso_list_get_length(arguments);
    if (argument_count > (SIZE_MAX / sizeof(char *)) - 2) {
        errorAndAbort("too many child process arguments");
    }

    char **result = malloc(sizeof(char *) * ((size_t)argument_count + 2));
    if (result == NULL) {
        errorAndAbort("failed to allocate child process arguments");
    }

    result[0] = yyvalue_to_string(program);
    yyvalue *argument_array = iso_list_get_elements(arguments);
    for (uint64_t i = 0; i < argument_count; ++i) {
        result[i + 1] = yyvalue_to_string(argument_array[i]);
    }
    result[argument_count + 1] = NULL;
    return result;
}

yyvalue yyRunProcessGetOutputSync(yyvalue program, yyvalue arguments) {
    char *program_name = yyvalue_to_string(program);
    char **argument_vector = make_argument_vector(program, arguments);
    output_buffer_t stdout_buffer = {0};
    output_buffer_t stderr_buffer = {0};
    int result = output_buffer_init(&stdout_buffer);
    if (result == 0) {
        result = output_buffer_init(&stderr_buffer);
    }
    if (result != 0) {
        free(argument_vector);
        output_buffer_destroy(&stdout_buffer);
        output_buffer_destroy(&stderr_buffer);
        errorAndAbort("failed to allocate child process output buffer");
    }

    pid_t pid;
    int stdout_fd;
    int stderr_fd;
    result = spawn_process(
        program_name,
        argument_vector,
        CHILD_STDIO_CAPTURE,
        &pid,
        &stdout_fd,
        &stderr_fd
    );
    free(argument_vector);
    if (result != 0) {
        fprintf(stderr, "posix_spawnp error: %s: %s\n", program_name, strerror(result));
        output_buffer_destroy(&stdout_buffer);
        output_buffer_destroy(&stderr_buffer);
        errorAndAbort("spawn error");
    }

    result = collect_process_output(stdout_fd, stderr_fd, &stdout_buffer, &stderr_buffer);
    int exit_status = 1;
    int wait_result = wait_for_process(pid, &exit_status);
    if (result == 0) {
        result = wait_result;
    }
    if (result != 0) {
        fprintf(stderr, "child process I/O error: %s\n", strerror(result));
        output_buffer_destroy(&stdout_buffer);
        output_buffer_destroy(&stderr_buffer);
        errorAndAbort("child process error");
    }

    yyvalue stdout_value = malloc_string_to_yyvalue(
        stdout_buffer.length + 1,
        stdout_buffer.data
    );
    yyvalue stderr_value = malloc_string_to_yyvalue(
        stderr_buffer.length + 1,
        stderr_buffer.data
    );
    output_buffer_destroy(&stdout_buffer);
    output_buffer_destroy(&stderr_buffer);

    yyvalue values[] = {
        bool_to_yyvalue(exit_status == 0),
        stdout_value,
        stderr_value,
    };
    return tuple_to_yyvalue(3, values);
}

yyvalue yyRunProcessSync(yyvalue program, yyvalue arguments) {
    char *program_name = yyvalue_to_string(program);
    char **argument_vector = make_argument_vector(program, arguments);
    pid_t pid;
    int unused_stdout_fd;
    int unused_stderr_fd;
    int result = spawn_process(
        program_name,
        argument_vector,
        CHILD_STDIO_IGNORE,
        &pid,
        &unused_stdout_fd,
        &unused_stderr_fd
    );
    free(argument_vector);
    if (result != 0) {
        fprintf(stderr, "posix_spawnp error: %s: %s\n", program_name, strerror(result));
        return bool_to_yyvalue(false);
    }

    int exit_status = 1;
    result = wait_for_process(pid, &exit_status);
    if (result != 0) {
        fprintf(stderr, "waitpid error: %s\n", strerror(result));
        return bool_to_yyvalue(false);
    }
    return bool_to_yyvalue(exit_status == 0);
}

yyvalue yyRunProcessSyncPipeOutput(yyvalue program, yyvalue arguments) {
    char *program_name = yyvalue_to_string(program);
    char **argument_vector = make_argument_vector(program, arguments);
    pid_t pid;
    int unused_stdout_fd;
    int unused_stderr_fd;
    int result = spawn_process(
        program_name,
        argument_vector,
        CHILD_STDIO_INHERIT_OUTPUT,
        &pid,
        &unused_stdout_fd,
        &unused_stderr_fd
    );
    free(argument_vector);
    if (result != 0) {
        fprintf(stderr, "posix_spawnp error: %s: %s\n", program_name, strerror(result));
        return int_to_yyvalue(result);
    }

    int exit_status = 1;
    result = wait_for_process(pid, &exit_status);
    if (result != 0) {
        fprintf(stderr, "waitpid error: %s\n", strerror(result));
        return int_to_yyvalue(result);
    }
    return int_to_yyvalue(exit_status);
}
