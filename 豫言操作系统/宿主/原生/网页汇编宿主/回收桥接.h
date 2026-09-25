/* 文言：此桥唯转值与行外务；编译之术悉在客中。汉语：复用现有系统原语，GC 客内完成编译、对象分配和续延执行。 */
#include <setjmp.h>
#ifdef YY_WASM_GC_BULK
/* 文言：字节整段往来。汉语：复制宿主扩展只包装 Wasmtime 自身的批量数组接口。 */
extern wasmtime_error_t *yy_wasmtime_arrayref_copy_bytes(wasmtime_context_t *, const wasmtime_arrayref_t *, uint8_t *, size_t);
extern wasmtime_error_t *yy_wasmtime_arrayref_from_bytes(wasmtime_context_t *, const wasmtime_array_ref_pre_t *, const uint8_t *, size_t, wasmtime_arrayref_t *);
#endif
extern void *(*豫言_外部分配器)(uint64_t);
typedef struct 临时分配块 { struct 临时分配块 *后; max_align_t 对齐; unsigned char 数据[]; } 临时分配块;
static 临时分配块 *桥临时块;
static void *桥分配(uint64_t 槽) {
    if (槽 > (SIZE_MAX - sizeof(临时分配块)) / sizeof(豫言值)) abort();
    临时分配块 *块 = calloc(1, sizeof(*块) + 槽 * sizeof(豫言值));
    if (!块) abort();
    块->后 = 桥临时块; 桥临时块 = 块; return 块->数据;
}
typedef struct {
    wasmtime_context_t *上下文;
    wasmtime_caller_t *客;
    wasmtime_array_ref_pre_t *字节预, *元组预;
    wasmtime_struct_ref_pre_t *整数预, *小数预;
    jmp_buf 失败;
    char 错误[1024];
} 回收转换;
static void 桥报错(回收转换 *桥, const char *文) {
    snprintf(桥->错误, sizeof 桥->错误, "%s", 文); longjmp(桥->失败, 1);
}
static void 桥检查(回收转换 *桥, wasmtime_error_t *错) {
    if (!错) return;
    wasm_name_t 文; wasmtime_error_message(错, &文);
    snprintf(桥->错误, sizeof 桥->错误, "%.*s", (int)文.size, 文.data);
    wasm_name_delete(&文); wasmtime_error_delete(错); longjmp(桥->失败, 1);
}
static void 桥准备(回收转换 *桥) {
    const char *名[] = {"yy_bytes", "yy_tuple", "yy_big", "yy_float"};
    for (int 序 = 0; 序 < 4; 序++) {
        wasmtime_extern_t 外; wasmtime_val_t 值;
        if (!wasmtime_caller_export_get(桥->客, 名[序], strlen(名[序]), &外) || 外.kind != WASMTIME_EXTERN_GLOBAL) 桥报错(桥, "GC 模块缺少类型原型");
        wasmtime_global_get(桥->上下文, &外.of.global, &值);
        if (序 < 2) {
            wasmtime_arrayref_t 组;
            if (!wasmtime_anyref_as_array(桥->上下文, &值.of.anyref, &组)) 桥报错(桥, "GC 数组原型无效");
            wasmtime_array_type_t *型 = wasmtime_arrayref_type(桥->上下文, &组);
            wasmtime_array_ref_pre_t *预 = wasmtime_array_ref_pre_new(桥->上下文, 型);
            if (序 == 0) 桥->字节预 = 预; else 桥->元组预 = 预;
            wasmtime_array_type_delete(型); wasmtime_arrayref_unroot(&组);
        } else {
            wasmtime_structref_t 组;
            if (!wasmtime_anyref_as_struct(桥->上下文, &值.of.anyref, &组)) 桥报错(桥, "GC 数值原型无效");
            wasmtime_struct_type_t *型 = wasmtime_structref_type(桥->上下文, &组);
            wasmtime_struct_ref_pre_t *预 = wasmtime_struct_ref_pre_new(桥->上下文, 型);
            if (序 == 2) 桥->整数预 = 预; else 桥->小数预 = 预;
            wasmtime_struct_type_delete(型); wasmtime_structref_unroot(&组);
        }
        wasmtime_val_unroot(&值);
    }
}
static 豫言值 客转原(回收转换 *桥, const wasmtime_val_t *值, unsigned 深) {
    if (深 > 512 || 值->kind != WASMTIME_ANYREF) 桥报错(桥, "GC 系统接口参数无效或嵌套过深");
    if (wasmtime_anyref_is_null(&值->of.anyref)) return 单元转豫言值();
    int32_t 整;
    if (wasmtime_anyref_i31_get_s(桥->上下文, &值->of.anyref, &整)) return 整数转豫言值(整);
    wasmtime_arrayref_t 组;
    if (wasmtime_anyref_as_array(桥->上下文, &值->of.anyref, &组)) {
        uint32_t 长; 桥检查(桥, wasmtime_arrayref_len(桥->上下文, &组, &长));
        wasmtime_array_type_t *型 = wasmtime_arrayref_type(桥->上下文, &组);
        wasmtime_field_type_t 域; wasmtime_array_type_element(型, &域);
        bool 字节 = 域.storage.kind == WASMTIME_STORAGE_TYPE_KIND_I8;
        wasmtime_field_type_delete(&域); wasmtime_array_type_delete(型);
        豫言值 原 = 字节 ? 分配豫言_字节串缓冲区(长) : 分配豫言元组(长);
#ifdef YY_WASM_GC_BULK
        if (字节) {
            桥检查(桥, yy_wasmtime_arrayref_copy_bytes(桥->上下文, &组, 豫言值转字节串指针(原), 长));
            wasmtime_arrayref_unroot(&组); return 原;
        }
#endif
        for (uint32_t 序 = 0; 序 < 长; 序++) {
            wasmtime_val_t 元; 桥检查(桥, wasmtime_arrayref_get(桥->上下文, &组, 序, &元));
            if (字节) 豫言值转字节串指针(原)[序] = (unsigned char)元.of.i32;
            else 写入元组(原, 序, 客转原(桥, &元, 深 + 1));
            wasmtime_val_unroot(&元);
        }
        wasmtime_arrayref_unroot(&组); return 原;
    }
    wasmtime_structref_t 构;
    if (wasmtime_anyref_as_struct(桥->上下文, &值->of.anyref, &构)) {
        wasmtime_val_t 域; 桥检查(桥, wasmtime_structref_field(桥->上下文, &构, 0, &域));
        豫言值 原;
        if (域.kind == WASMTIME_I64) 原 = 整数转豫言值(域.of.i64);
        else if (域.kind == WASMTIME_F64) 原 = 小数转豫言值(域.of.f64);
        else if (域.kind == WASMTIME_I32) {
            wasmtime_val_t 载; 桥检查(桥, wasmtime_structref_field(桥->上下文, &构, 1, &载));
            原 = 客转原(桥, &载, 深 + 1); uint64_t 长 = 获取豫言元组长度(原);
            设置豫言值类型(&原, 13); 设置豫言值子类型(&原, 长); 设置豫言值原始长度(&原, 域.of.i32);
            wasmtime_val_unroot(&载);
        } else { 桥报错(桥, "未知 GC 结构值"); 原 = 0; }
        wasmtime_val_unroot(&域); wasmtime_structref_unroot(&构); return 原;
    }
    桥报错(桥, "未知 GC 参数值"); return 0;
}
static wasmtime_val_t 原转客(回收转换 *桥, 豫言值 原, unsigned 深) {
    if (深 > 512) 桥报错(桥, "GC 系统接口返回值嵌套过深");
    wasmtime_val_t 值 = {.kind = WASMTIME_ANYREF};
    uint64_t 型 = 获取豫言值类型(原);
    if (型 == 空值类型标记) { wasmtime_anyref_set_null(&值.of.anyref); return 值; }
    if (型 == 整数类型标记 || 型 == 爻类型标记 || 型 == 小数类型标记) {
        int64_t 整 = 豫言值转整数(原);
        if (型 != 小数类型标记 && 整 >= -1073741824 && 整 <= 1073741823) {
            wasmtime_anyref_from_i31(桥->上下文, (uint32_t)整, &值.of.anyref); return 值;
        }
        wasmtime_val_t 域 = {.kind = 型 == 小数类型标记 ? WASMTIME_F64 : WASMTIME_I64};
        if (型 == 小数类型标记) 域.of.f64 = 豫言值转小数(原); else 域.of.i64 = 整;
        wasmtime_structref_t 构;
        桥检查(桥, wasmtime_structref_new(桥->上下文, 型 == 小数类型标记 ? 桥->小数预 : 桥->整数预, &域, 1, &构));
        wasmtime_structref_to_anyref(&构, &值.of.anyref); wasmtime_structref_unroot(&构); return 值;
    }
    bool 字节 = 豫言值是字节串(原);
    if (!字节 && !豫言值是元组(原)) 桥报错(桥, "系统接口返回了不支持的原生对象类型");
    uint64_t 长 = 字节 ? 获取豫言_字节串长度(原) : 获取豫言元组长度(原);
    if (长 > UINT32_MAX) 桥报错(桥, "系统接口返回对象超过 Wasm GC 长度上限");
    wasmtime_arrayref_t 组; wasmtime_val_t 零 = {.kind = 字节 ? WASMTIME_I32 : WASMTIME_ANYREF};
#ifdef YY_WASM_GC_BULK
    if (字节) {
        桥检查(桥, yy_wasmtime_arrayref_from_bytes(桥->上下文, 桥->字节预, 豫言值转字节串指针(原), 长, &组));
        wasmtime_arrayref_to_anyref(&组, &值.of.anyref); wasmtime_arrayref_unroot(&组); return 值;
    }
#endif
    桥检查(桥, wasmtime_arrayref_new(桥->上下文, 字节 ? 桥->字节预 : 桥->元组预, &零, (uint32_t)长, &组));
    for (uint32_t 序 = 0; 序 < 长; 序++) {
        wasmtime_val_t 元;
        if (字节) { 元.kind = WASMTIME_I32; 元.of.i32 = 豫言值转字节串指针(原)[序]; }
        else 元 = 原转客(桥, 读取元组(原, 序), 深 + 1);
        桥检查(桥, wasmtime_arrayref_set(桥->上下文, &组, 序, &元)); wasmtime_val_unroot(&元);
    }
    wasmtime_arrayref_to_anyref(&组, &值.of.anyref); wasmtime_arrayref_unroot(&组); return 值;
}
#include "回收原语.h"
static 豫言值 回收系统调用(回收转换 *桥, 宿主状态 *状态, const char *名, 豫言值 *参, size_t 数) {
    if (!strcmp(名, "豫言_获取命令行程序名")) return 静态字符串转豫言值(状态->模块);
    if (!strcmp(名, "豫言_获取命令行参数")) {
        uint64_t 总 = 获取同构列长度(状态->客参数);
        豫言值 *诸参 = 获取同构列元素(状态->客参数);
        return 数组转同构列(总 - 状态->参数起, 诸参 + 状态->参数起);
    }
    if (数 == 2 && (!strncmp(名, "豫言_同步运行子进程", strlen("豫言_同步运行子进程")) || !strcmp(名, "豫言_启动异步子进程"))) {
        const char *程序 = 豫言值转字符串(参[0]);
        bool 预编译 = 状态->预编译 && !strcmp(程序, 状态->模块);
        if (是模块(程序) || 预编译) {
            size_t 长 = 获取同构列长度(参[1]), 前 = 预编译 ? 2 : 1;
            豫言值 *诸参 = 桥分配(长 + 前);
            if (预编译) 诸参[0] = 静态字符串转豫言值("--运行预编译");
            诸参[前 - 1] = 参[0];
            memcpy(诸参 + 前, 获取同构列元素(参[1]), 长 * sizeof(豫言值));
            参[1] = 数组转同构列(长 + 前, 诸参);
            参[0] = 静态字符串转豫言值(状态->运行器);
        }
    }
    return 调原语(桥, 名, 参, 数);
}
static wasm_trap_t *回收宿主回调(void *数据, wasmtime_caller_t *客, const wasmtime_val_t *参数, size_t 数, wasmtime_val_t *结果, size_t 结果数) {
    (void)数; (void)结果数;
    回收转换 *桥 = calloc(1, sizeof(*桥)); if (!桥) abort();
    桥->客 = 客; 桥->上下文 = wasmtime_caller_context(客);
    void *(*旧分配器)(uint64_t) = 豫言_外部分配器;
    豫言_外部分配器 = 桥分配;
    wasm_trap_t *陷阱 = NULL;
    if (!setjmp(桥->失败)) {
        桥准备(桥);
        豫言值 名值 = 客转原(桥, &参数[0], 0), 参值 = 客转原(桥, &参数[1], 0);
        if (!豫言值是字节串(名值) || !豫言值是元组(参值)) 桥报错(桥, "GC 宿主调用格式错误");
        const char *名 = 豫言值转字符串(名值);
        豫言值 返回 = 回收系统调用(桥, 数据, 名, 豫言值转元组(参值), 获取豫言元组长度(参值));
        结果[0] = 原转客(桥, 返回, 0);
    } else 陷阱 = wasmtime_trap_new(桥->错误, strlen(桥->错误));
    豫言_外部分配器 = 旧分配器;
    while (桥临时块) { 临时分配块 *块 = 桥临时块; 桥临时块 = 块->后; free(块); }
    if (桥->字节预) wasmtime_array_ref_pre_delete(桥->字节预);
    if (桥->元组预) wasmtime_array_ref_pre_delete(桥->元组预);
    if (桥->整数预) wasmtime_struct_ref_pre_delete(桥->整数预);
    if (桥->小数预) wasmtime_struct_ref_pre_delete(桥->小数预);
    free(桥); return 陷阱;
}
static wasmtime_error_t *定义回收宿主(wasm_engine_t *引擎, wasmtime_linker_t *链接器, 宿主状态 *状态) {
    wasmtime_valtype_t 型 = {.kind = WASMTIME_VALTYPE_KIND_REF, .reftype = {.nullable = true, .heaptype = {.kind = WASMTIME_HEAPTYPE_KIND_EQ}}};
    wasm_valtype_t *入[] = {wasmtime_valtype_to_wasm(引擎, &型), wasmtime_valtype_to_wasm(引擎, &型)};
    wasm_valtype_t *出[] = {wasmtime_valtype_to_wasm(引擎, &型)};
    wasm_valtype_vec_t 入组, 出组; wasm_valtype_vec_new(&入组, 2, 入); wasm_valtype_vec_new(&出组, 1, 出);
    wasm_functype_t *类型 = wasm_functype_new(&入组, &出组);
    const char *接口 = "yuyan:gc-host/v1";
    wasmtime_error_t *错 = wasmtime_linker_define_func(链接器, 接口, strlen(接口), "call", 4, 类型, 回收宿主回调, 状态, NULL);
    wasm_functype_delete(类型); return 错;
}
/* 文言：先张其堆而释所借，量由外设。汉语：C API 未暴露 GC 初始容量设置；可通过一次临时分配预留容量，随后立即回收，开销计入启动时间。 */
static wasmtime_error_t *预留回收堆(wasmtime_context_t *上下文, const wasmtime_instance_t *实例) {
    const char *设 = getenv("YY_WASM_GC_RESERVE_MB");
    if (!设) return NULL;
    char *尾; long 数 = strtol(设, &尾, 10);
    if (*尾 || 数 < 0 || 数 > 512) return wasmtime_error_new("YY_WASM_GC_RESERVE_MB 须为零至五百一十二");
    if (!数) return NULL;
    wasmtime_extern_t 外;
    if (!wasmtime_instance_export_get(上下文, 实例, "yy_bytes", 8, &外) || 外.kind != WASMTIME_EXTERN_GLOBAL) return NULL;
    wasmtime_val_t 原型; wasmtime_global_get(上下文, &外.of.global, &原型);
    wasmtime_arrayref_t 原型组; if (!wasmtime_anyref_as_array(上下文, &原型.of.anyref, &原型组)) { wasmtime_val_unroot(&原型); return wasmtime_error_new("GC 字节原型无效"); }
    wasmtime_array_type_t *型 = wasmtime_arrayref_type(上下文, &原型组);
    wasmtime_array_ref_pre_t *预 = wasmtime_array_ref_pre_new(上下文, 型);
    wasmtime_arrayref_t 临时; wasmtime_val_t 零 = {.kind = WASMTIME_I32};
    wasmtime_error_t *错 = wasmtime_arrayref_new(上下文, 预, &零, (uint32_t)数 * 1024 * 1024, &临时);
    if (!错) wasmtime_arrayref_unroot(&临时);
    wasmtime_array_ref_pre_delete(预); wasmtime_array_type_delete(型); wasmtime_arrayref_unroot(&原型组); wasmtime_val_unroot(&原型);
    if (!错) wasmtime_context_gc(上下文);
    return 错;
}
