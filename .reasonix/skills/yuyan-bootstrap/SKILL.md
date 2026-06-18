---
name: yuyan-bootstrap
description: How to bootstrap the Yuyan self-hosting compiler — 5-stage chain, common pitfalls, build commands, and pipeline architecture
---

# Yuyan Compiler Bootstrap Guide

## Bootstrap Chain

The Yuyan self-hosting compiler is built through a 5-stage bootstrap chain:

```
yy (SML, MLton) → yy_bs → yy_bs_bs → yy_bs_bs_bs → yy_bs_bs_bs_bs → yy_bs_bs_bs_bs_bs
```

| Stage | Built by | Speed | Notes |
|-------|----------|-------|-------|
| `yy` | MLton (`src/`) | fast | SML bootstrap compiler. One-time build from `src/development.mlb`. |
| `yy_bs` | `yy` | ~400s | First Yuyan-compiled compiler. Uses SML codegen (i64, monolithic .ll). |
| `yy_bs_bs` | `yy_bs` | ~slow | Self-hosting codegen (i128 tagged, modular .o files). First stage with full optimization pipeline. |
| `yy_bs_bs_bs` | `yy_bs_bs` | faster | Optimization passes compound. `--parallel` works here. |
| `yy_bs_bs_bs_bs` | `yy_bs_bs_bs` | ~35s | Fast enough for development. Use `--parallel --static-linking`. |

Bootstrap principle: each stage compiles the same source (`豫言编译器/入口。豫`) to produce the next stage. Stages 3+ should produce **identical binaries** (verified by hash).

## Prerequisites

```bash
# macOS (Apple Silicon)
brew install mlton bdw-gc libuv llvm

# Ensure llvm-as is in PATH
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
```

## Step 1: Build `yy` (SML bootstrap compiler)

```bash
cd /Users/zc/repos/yuyan
make yy
```

**Common issues:**
- `JSONUtil.FieldNotFound` API mismatch → fix: change `FieldNotFound (jv, s)` to `FieldNotFound s` in `src/lsp/language-server-mode.sml`
- Missing SML libraries → verify MLton installation and `$(SML_LIB)` path

## Step 2: Build the runtime library

Two runtime directories exist:
- `runtime/` — used by SML `yy` (old entry point, `-D OLD_ENTRY`)
- `运行时支持库/` — used by self-hosting stages

```bash
# Build both
make -C runtime/ debug_old
make -C ./运行时支持库 all VERSION=v0.1.0rc2+0019
```

**Common issues:**
- `gc.h` not found → the Makefiles use `-I /usr/local/include`. On Apple Silicon, add `-I /opt/homebrew/include`. Both Makefiles should already be patched.
- `-Werror` failures on newer clang → change to `-Wno-error`. Both Makefiles should already be patched.

## Step 3: Build `yy_bs` (first self-hosting stage)

```bash
./yy -c --use-local-lib 豫言编译器/入口。豫 -o yy_bs
```

This generates a ~286MB monolithic `.ll` file in `.yybuild.nosync/`, then invokes clang.

**Common issues:**
- `musttail call` failures on newer clang (83K errors) → the SML codegen emits `musttail` calls that don't satisfy LLVM's strict matching requirements. Fix:
  ```bash
  sed -i '' 's/musttail call/tail call/g' .yybuild.nosync/yy*.ll
  ```
  Then run clang manually:
  ```bash
  clang .yybuild.nosync/yy*.ll runtime/libyyrtdebug_old.a -g -o yy_bs \
    -L /opt/homebrew/lib -l gc -l uv -Wno-override-module
  ```
- Library paths → add `-L /opt/homebrew/lib` for Apple Silicon

## Step 4: Build `yy_bs_bs` (second stage, uses self-hosting codegen)

```bash
# Fix rpath on yy_bs first
install_name_tool -change libyyrtoptv0.1.0rc2+0019.dylib \
  @loader_path/运行时支持库/libyyrtoptv0.1.0rc2+0019.dylib yy_bs

# Build. Add --do-not-optimize to skip LLVM optimization passes for faster build
./yy_bs 豫言编译器/入口。豫 -o yy_bs_bs -c --do-not-optimize
```

**Common issues:**
- `libyyrtopt...dylib` not found at runtime → `yy_bs` was linked with `-Wl,-rpath,./运行时支持库` which macOS dyld doesn't resolve. Use `install_name_tool` above. The fix is in `主程序生成。豫` source (absolute rpath), but that requires recompiling `yy_bs` from scratch.
- `llvm-as: no such file or directory` → ensure `llvm` from homebrew is in PATH

## Step 5: Build faster stages

```bash
# yy_bs_bs_bs — ~35s with parallel + static linking
./yy_bs_bs 豫言编译器/入口。豫 -o yy_bs_bs_bs -c --parallel --static-linking

# yy_bs_bs_bs_bs — even faster
./yy_bs_bs_bs 豫言编译器/入口。豫 -o yy_bs_bs_bs_bs -c --parallel --static-linking

# yy_bs_bs_bs_bs_bs
./yy_bs_bs_bs_bs 豫言编译器/入口。豫 -o yy_bs_bs_bs_bs_bs -c --parallel --static-linking --optimize
```

**Key flags:**
- `-c` — compile only, don't run
- `--parallel` — invoke `parallel_compile.py` for multi-file parallel compilation
- `--static-linking` — link `.a` file instead of `.dylib` (avoids rpath issues)
- `--do-not-optimize` — skip LLVM `opt` passes (faster build, slower binary)
- `--optimize` — enable `opt -O3` and LTO (slower build, faster binary)
- `--debug` — generate debug info

## Self-hosting compilation pipeline

The self-hosting compiler (`yy_bs` and beyond) uses this pipeline per file:

```
Parse → TypeCheck → TypeErase → ANF(无类型正则变换) → CPS(续延传递变换)
→ ClosureConvert(前闭包转换) → FunctionHoist(函数提升)
→ CrossModuleOpt(跨文件优化: 去柯里化, 常量内联, 函数参数优化)
→ LocalOpt(本地优化: 栈分配, 尾调用, 树组优化)
→ CodegenPrep(代码生成准备: 栈帧布局, GC检查注入)
→ LLVM Codegen(emit i128-tagged LLVM IR)
→ llvm-as → opt → llc → clang .o → link
```

Optimization passes compound across stages: each stage's compiler has all previous optimizations baked in, so `yy_bs_bs_bs` compiles faster than `yy_bs_bs`.

## Parallel compilation architecture

`parallel_compile.py` breaks compilation into stages:
1. `dependency-analysis` — resolve imports
2. `parse` — lex + parse each file
3. `type-check-and-erase` — bidirectional type check + erase types
4. `cross-module-optimize` — uncurrying, constant inlining
5. `pre-closure-convert` — closure conversion prep
6. `anf` — A-Normal Form + CPS transform
7. `all-codegen` — LLVM IR gen + llvm-as + llc → .o

Each stage processes files in parallel using `ProcessPoolExecutor`. Stages are pipelined: a file can be in codegen while another is still parsing. Dependencies are tracked so a file waits for its imports to complete each stage before proceeding.

To debug parallel build issues, check:
- `yy_parallel_log.txt` — detailed worker logs
- `yy_parallel_deps.txt` — dependency graph

## Key source files

| File | Role |
|------|------|
| `豫言编译器/入口。豫` | Compiler entry point, CLI argument handling |
| `豫言编译器/编译步骤/类型检查/对象类型检查。豫` | Bidirectional type checker (55K, largest file) |
| `豫言编译器/编译步骤/语法分析/抽象语法分析。豫` | Mixfix parser |
| `豫言编译器/编译步骤/求值正则变换/无类型正则变换。豫` | ANF transform |
| `豫言编译器/编译步骤/闭包转换/正则前闭包转换。豫` | Closure conversion |
| `豫言编译器/编译步骤/代码生成准备变换/代码生成准备变换。豫` | Stack frame + GC injection |
| `豫言编译器/编译步骤/代码生成/低级虚拟机/直接代码生成。豫` | i128 LLVM IR emitter |
| `豫言编译器/编译步骤/代码生成/低级虚拟机/主程序生成。豫` | Link command generation, main entry |
| `豫言编译器/编译步骤/总体过程/编译过程。豫` | Pipeline orchestration |
| `parallel_compile.py` | Parallel build orchestrator |
| `src/passes/codegen/llvm-codegen.sml` | SML codegen (i64, only used by `yy`) |
| `src/passes/cps-transform/cps-pass.sml` | SML CPS transform (only used by `yy`) |

## Makefile targets

```bash
make yy              # Build SML bootstrap compiler
make yy_bs           # Build first self-hosting stage
make yy_bs_bs        # Build second stage (serial)
make yy_bs_bs_parallel # Build second stage (parallel)
make yy_bs_bs_bs     # Build third stage
make yy_bs_bs_bs_bs  # Build fourth stage
make yy_bs_bs_bs_bs_bs # Build fifth stage
make yyrt            # Build old runtime library
make yy_runtime_lib  # Build self-hosting runtime library
make test            # Run tests
make clean           # Remove yy
make cleanbs         # Remove yy_bs
make superclean      # Remove all build artifacts + caches
```

## Common pitfalls

1. **`衔` vs `附`**: `衔` is list concatenation (from `多态列`), `附` is string concatenation (from `字符串术`). Using the wrong one causes type unification failures.
2. **rpath on macOS**: `-Wl,-rpath,./dir` doesn't work with dyld. Use absolute paths or `@loader_path`.
3. **musttail on new LLVM**: SML codegen emits `musttail call` that fails verification on LLVM 16+. Fix: `sed 's/musttail call/tail call/g'` on the .ll file.
4. **i128 vs i64**: SML codegen uses `i64`, self-hosting codegen uses `i128` (tagged union).
5. **`--debug` flag**: Not recognized by SML `yy`, use `--super-verbose` for `yy`. For self-hosting stages, `--debug` works and `-v`/`-vv`/`-vvv` control verbosity.
6. **First build is slowest**: The SML `yy` codegen produces a 286MB monolithic .ll file. clang compilation alone takes ~400s. Subsequent stages use modular `.o` files which link much faster.
7. **Python version**: `parallel_compile.py` needs Python 3.12+ (uses new `ProcessPoolExecutor` features).
8. **Cache directory**: `.yybuild.v0.1.0rc2+0019.nosync/` — contains per-file `.o`, `.ll`, `.bc` files and JSON caches. Delete to force full rebuild.
