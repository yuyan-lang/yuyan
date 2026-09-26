# WASI 示例：于豫言操作系统中行 C 程序

`工具/豫言裸机/WASI示例/` 乃一目录之树，可径置于磁盘：`程序/` 下十五 `.wasm`，皆 wasi-sdk 所编（C 程序依 WASI preview1 之约，导入 `wasi_snapshot_preview1`，导出 `_start`），`源码/` 下其 C 源文件也。壳测试 `WASI` 与 `WASI文件` 以符号链接令此树为预置之树，故每程序于二架构之 QEMU 中，出与退出码皆逐字验之。执行器所作 WASI 之何部，见 [客体二号](客体二号.文言.md) 之“WASI preview1 之子集”一节。

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
壳> 运行 /程序/文件测试.wasm
mkdir /测试 成功
mkdir 重复 失败：File exists
读到：第一行
……
壳> 运行 /程序/猫.wasm /测试/甲.txt
改一行
第二行 42
追加行
壳> 运行 /程序/回声.wasm
回声就绪
hello
1: HELLO
```

管与重定向亦可用：`运行 /程序/猫.wasm /数据/文本.txt | /程序/大写.wasm | /程序/字数.wasm`、`运行 /程序/字数.wasm < /数据/文本.txt`、`设 问候 你好` 后 `运行 /程序/环境.wasm 问候`（`/数据/文本.txt` 乃此树中一小文本，壳测试 `管道` 用之）。

末例之 `hello` 乃汝所敲（由 `读行核` 回显），`1: HELLO` 乃程序之出；于行首按 Ctrl-D 而止输入，程序印其统计而退。程序中之路径，皆相对于预开之根目录 “/”，故 `/测试/甲.txt` 即盘上同一文件，壳之 `读`、`列` 亦见之。

## 诸程序

| 程序 | 所为 | 所示之能 |
|---|---|---|
| `你好` | 印参数之数与诸参数、数浮点数，以退出码三终 | 命令行参数、`printf("%f")`、libm、`proc_exit` |
| `回声` | 逐行读标准入，易为大写而编号 | 标准入（回显、行首 Ctrl-D 为文毕） |
| `计算` | 筛素数、qsort、二叉搜索树、八皇后、SHA-256、浮点与整数之格式化 | 整数与浮点指令、`malloc` 与 `memory.grow`、六十四位整数；出确定，可与 Node 自带 WASI 逐字相校 |
| `记时` | 单调之钟不退、分辨率为正、二次 `getentropy` 相异 | `clock_time_get`、`clock_res_get`、`random_get` |
| `睡眠` | `usleep`、`nanosleep`、`sleep` 之后以单调之钟核其睡足 | `poll_oneoff` 之时钟订阅 |
| `出入` | 标准出与标准错交替、`atexit`、逐印诸参数、以首参为退出码 | `fd_write` 之二流、退出码归于壳 |
| `陷阱 [1–4]` | 一 `abort`、二整数除零、三线性内存越界、四间接调用越界 | 陷阱唯止子任务，壳续行 |
| `猫 文件…` | 依次写诸文件于标准出，不能开者报于标准错而以一退 | 读文件（`path_open`、`fd_read`）、`fopen` 败之错讯 |
| `目录转储 [路径]` | 递归列目录之树，文件示其长与 FNV-1a 之校验 | `opendir`、`readdir`、`stat` |
| `文件测试` | 建目录、写读追加、二进制万字节、`fseek`、列目录、改名、删、错讯、未 `fclose` 之文件 | 文件与目录之综合，`_start` 返时写回未关之文件 |
| `文件边界` | `O_EXCL`、`O_APPEND`、`pread`、`pwrite`、`ftruncate`、稀疏之文件、读写目录、路径中之 `..`、父路径为文件、`rename` 之覆盖、`chdir` | 文件语义之界与错误号（与 Linux 同） |
| `字数` | 读标准入至毕，印行数、词数、字节数（wc） | 标准入之重定向（`< 文件` 或管），字节无差 |
| `大写` | 读标准入，易小写为大写而出于标准出（tr） | 管（`猫 文件 | 大写 | 字数`） |
| `环境` | 依名序印全部环境变量，复对每一参数印 `getenv` | 环境变量（壳之 `设` 令所予） |
| `文件压力` | 写百千字节之文件，再整读四百次 | 文件缓冲之垃圾回收（唯于 Node 夹具中行：子任务中文件走邮箱，搬四十兆字节太缓） |

## 自编

用 [wasi-sdk](https://github.com/WebAssembly/wasi-sdk)（示例所用者 clang 二十三点一点零之版），目标 `wasm32-wasip1`：

```sh
wasi-sdk/bin/clang --target=wasm32-wasip1 -Os -Wl,--strip-all -o 程序/你好.wasm 源码/你好.c
```

`-Wl,--strip-all` 去名字段与调试之信（`你好.wasm` 由约一百六十千字节缩为二十七千）。产物入盘之后不须转换；执行器视之如常模块而解析之，唯其导入 `wasi_snapshot_preview1` 之函数由 `WASI` 模块直作。Rust 之 `wasm32-wasip1` 目标同理（未专验）。

## 校

以 Node 自带之 WASI 行同一 `.wasm` 而得参照之出（`node:wasi` 之 `WASI({ version: 'preview1', args, preopens: { '/': 临时目录 } })`，标准流接于管道），与执行器之出逐字相较；`工具/豫言裸机/客体二号/WASI对照.cjs` 作其不带磁盘之半（命令行、标准流、计算、时钟、陷阱），带磁盘之文件试则于 Node 中以 `虚拟块设备` 夹具行之，复以一 `目录转储` 读同一块盘而相校：

- `计算` 于 `-O0`、`-O1`、`-O2`、`-Os`、`-O3` 下皆同；
- `出入`、`你好` 之参数与退出码同；
- `陷阱` 之四陷阱，于 Node 为 `RuntimeError`（unreachable、divide by zero、memory access out of bounds、table index is out of bounds），于执行器为相应之退出码一百二十八、一百二十五、一百二十七、一百二十四；
- `文件测试`、`文件边界` 之出与 Node 逐字相同，唯三处宿主特有之异：Node 报目录之大小（宿主文件系统之一百二十八）、对目录 `read` 得 EBADF（执行器与 Linux 同为 EISDIR）、`unlink` 目录得 EPERM（执行器为 EISDIR）。

此校曾发二实患：`计算` 中之二叉树，于执行器中高少算一层，因未声明上限之内存不得 `memory.grow`；文件试于 QEMU 之子任务中偶败，因内核邮箱唯容一信，紧接之二远程文件调用会遇“邮箱满”（见“WASI preview1 之子集”一节之“内存之长”与“邮箱发送之重试”）。
