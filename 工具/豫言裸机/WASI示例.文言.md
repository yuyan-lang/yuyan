# WASI 示例：于豫言操作系统中行 C 程序

`工具/豫言裸机/WASI示例/` 乃一目录之树，可径置于磁盘：`程序/` 下六 `.wasm`，皆 wasi-sdk 所编（C 程序依 WASI preview1 之约，导入 `wasi_snapshot_preview1`，导出 `_start`），`源码/` 下其 C 源文件也。壳测试 `WASI` 以符号链接令此树为预置之树，故每程序于二架构之 QEMU 中，出与退出码皆逐字验之。执行器所作 WASI 之何部，见 [客体二号](客体二号.文言.md) 之“WASI preview1 之子集”一节。

## 行

```sh
./yy豫构 文件 工具/豫言裸机/豫言系统。豫 --编译器 ./yy4_bs --输出 yy豫言系统
YY_GC_INITIAL_HEAP_SIZE_MB=1024 ./yy豫言系统 运行 --预置 工具/豫言裸机/WASI示例
```

启后于壳中以 `运行 路径 参数…` 起程序，以 `退出` 闭系统：

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

末例之 `hello` 乃汝所敲（由 `读行核` 回显），`1: HELLO` 乃程序之出；于行首按 Ctrl-D 而止输入，程序印其统计而退。

## 诸程序

| 程序 | 所为 | 所示之能 |
|---|---|---|
| `你好` | 印参数之数与诸参数、数浮点数，以退出码三终 | 命令行参数、`printf("%f")`、libm、`proc_exit` |
| `回声` | 逐行读标准入，易为大写而编号 | 标准入（回显、行首 Ctrl-D 为文毕） |
| `计算` | 筛素数、qsort、二叉搜索树、八皇后、SHA-256、浮点与整数之格式化 | 整数与浮点指令、`malloc` 与 `memory.grow`、六十四位整数；出确定，可与 Node 自带 WASI 逐字相校 |
| `记时` | 单调之钟不退、分辨率为正、二次 `getentropy` 相异 | `clock_time_get`、`clock_res_get`、`random_get` |
| `出入` | 标准出与标准错交替、`atexit`、逐印诸参数、以首参为退出码 | `fd_write` 之二流、退出码归于壳 |
| `陷阱 [1–4]` | 一 `abort`、二整数除零、三线性内存越界、四间接调用越界 | 陷阱唯止子任务，壳续行 |

## 自编

用 [wasi-sdk](https://github.com/WebAssembly/wasi-sdk)（示例所用者 clang 二十三点一点零之版），目标 `wasm32-wasip1`：

```sh
wasi-sdk/bin/clang --target=wasm32-wasip1 -Os -Wl,--strip-all -o 程序/你好.wasm 源码/你好.c
```

`-Wl,--strip-all` 去名字段与调试之信（`你好.wasm` 由约一百六十千字节缩为二十七千）。产物入盘之后不须转换；执行器视之如常模块而解析之，唯其导入 `wasi_snapshot_preview1` 之函数由 `WASI` 模块直作。Rust 之 `wasm32-wasip1` 目标同理（未专验）。

## 校

以 Node 自带之 WASI 行同一 `.wasm` 而得参照之出（`node:wasi` 之 `WASI({ version: 'preview1', args })`，标准流接于管道），与执行器之出逐字相较：

- `计算` 于 `-O0`、`-O1`、`-O2`、`-Os`、`-O3` 下皆同；
- `出入`、`你好` 之参数与退出码同；
- `陷阱` 之四陷阱，于 Node 为 `RuntimeError`（unreachable、divide by zero、memory access out of bounds、table index is out of bounds），于执行器为相应之退出码一百二十八、一百二十五、一百二十七、一百二十四。

此校曾发一实患：`计算` 中之二叉树，于执行器中高少算一层，因未声明上限之内存不得 `memory.grow`（见“WASI preview1 之子集”一节之“内存之长”）。
