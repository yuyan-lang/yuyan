# WASI 示例：在豫言操作系统里运行 C 程序

`工具/豫言裸机/WASI示例/` 是一棵可以直接放进磁盘的目录树：`程序/` 下是六个用 wasi-sdk 编成的 `.wasm`（C 程序按 WASI preview1 约定导入 `wasi_snapshot_preview1`、导出 `_start`），`源码/` 下是它们的 C 源文件。壳测试 `WASI` 用符号链接把它当作预置树，所以每个程序都在两个架构的 QEMU 里逐字验证过输出与退出码。执行器实现了 WASI 的哪一部分，见 [客体二号](客体二号.汉语.md) 的“WASI preview1 子集”一节。

## 运行

```sh
./yy豫构 文件 工具/豫言裸机/豫言系统。豫 --编译器 ./yy4_bs --输出 yy豫言系统
YY_GC_INITIAL_HEAP_SIZE_MB=1024 ./yy豫言系统 运行 --预置 工具/豫言裸机/WASI示例
```

启动后在壳里用 `运行 路径 参数…` 启动程序，用 `退出` 关掉系统：

```
壳> 运行 /程序/你好.wasm 甲 乙
hello wasi 3
arg0=/程序/你好.wasm
arg1=甲
arg2=乙
pi=3.141593 sqrt2=1.4142135624 sin1=0.8414709848
退出码：3
壳> 运行 /程序/回声.wasm
回声就绪
hello
1: HELLO
```

最后一例里的 `hello` 是你敲的（由 `读行核` 回显），`1: HELLO` 是程序的输出；在行首按 Ctrl-D 结束输入，程序打印统计后退出。

## 程序

| 程序 | 作用 | 展示的能力 |
|---|---|---|
| `你好` | 打印参数个数与各参数、几个浮点数，以退出码 3 结束 | 命令行参数、`printf("%f")`、libm、`proc_exit` |
| `回声` | 逐行读标准输入，转大写并编号 | 标准输入（回显、行首 Ctrl-D 为文件结束） |
| `计算` | 素数筛、qsort、二叉搜索树、八皇后、SHA-256、浮点与整数格式化 | 整数与浮点指令、`malloc` 与 `memory.grow`、64 位整数；输出确定，可与 Node 自带 WASI 逐字对照 |
| `记时` | 单调时钟不倒退、分辨率为正、两次 `getentropy` 不同 | `clock_time_get`、`clock_res_get`、`random_get` |
| `出入` | 标准输出与标准错误交替、`atexit`、逐个打印参数、以第一个参数为退出码 | `fd_write` 的两个流、退出码传回壳 |
| `陷阱 [1–4]` | 1 `abort`、2 整数除零、3 线性内存越界、4 间接调用越界 | 陷阱只结束子任务，壳继续执行 |

## 自己编译

用 [wasi-sdk](https://github.com/WebAssembly/wasi-sdk)（示例用的是 clang 23.1.0 的版本），目标 `wasm32-wasip1`：

```sh
wasi-sdk/bin/clang --target=wasm32-wasip1 -Os -Wl,--strip-all -o 程序/你好.wasm 源码/你好.c
```

`-Wl,--strip-all` 去掉名字段与调试信息（`你好.wasm` 由约 160 KB 缩到 27 KB）。产物放进磁盘后不需要任何转换；执行器把它当作普通模块解析，只是导入 `wasi_snapshot_preview1` 的函数由 `WASI` 模块直接实现。Rust 的 `wasm32-wasip1` 目标同理（未作专门验证）。

## 核对

用 Node 自带的 WASI 运行同一份 `.wasm` 得到参照输出（`node:wasi` 的 `WASI({ version: 'preview1', args })`，标准流接到管道），与执行器的输出逐字比较：

- `计算` 在 `-O0`、`-O1`、`-O2`、`-Os`、`-O3` 下都相同；
- `出入`、`你好` 的参数与退出码相同；
- `陷阱` 的四种陷阱在 Node 里是 `RuntimeError`（unreachable、divide by zero、memory access out of bounds、table index is out of bounds），在执行器里是对应的退出码 128、125、127、124。

这样的对照发现过一个真实的问题：`计算` 里的二叉树在执行器里高度少算一层，原因是没有声明上限的内存不能 `memory.grow`（见“WASI preview1 子集”一节的“内存增长”）。
