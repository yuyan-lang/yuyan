/* 文言：诸语同取单调钟。汉语：OCaml 计时使用与 C 相同的单调墙钟；分配返回浮点数，不声明 noalloc。 */
#include <time.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
CAMLprim value yy_monotonic_seconds(value unit) {
    CAMLparam1(unit);
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    CAMLreturn(caml_copy_double(t.tv_sec + t.tv_nsec * 1e-9));
}
