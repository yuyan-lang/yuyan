编译器入口 := 豫言编译器/入口。豫
网页汇编宿主编译器 ?= ./yy编译器
稳定编译器 := ./yy_bs_stable
稳定豫构 := ./yy豫构_stable
编译器源码 := $(shell rg --files 豫言编译器 库 工具/豫构 -g '*。豫' -g '*.yuyan' -g '*.c' -g '*.h' -g '!**/*_v0/**')
版本 := $(shell sed -n -E 's/^.*v(.*)\+([0-9]{4}).*/v\1+\2/p' 库/编译器核心/编译辅助工具/命令行/版本管理。豫)

.PHONY: 全部 类型检查 连续自举 自举包 豫构 豫构测试 运行时 yy_runtime_lib 清理生成数据

全部: 连续自举 豫构

类型检查: yy豫构
	./yy豫构 类型检查 豫言编译器 --编译器 ./yy4_bs

yy2_bs: $(编译器源码) yy_bs_stable yy豫构_stable | 运行时
	$(稳定豫构) 构建 豫言编译器 --编译器 $(稳定编译器) --输出 $@

yy3_bs: $(编译器源码) yy2_bs yy豫构_stable
	./yy豫构_stable 构建 豫言编译器 --编译器 ./yy2_bs --输出 $@

yy4_bs: $(编译器源码) yy3_bs yy豫构_stable
	./yy豫构_stable 构建 豫言编译器 --编译器 ./yy3_bs --输出 $@

yy2: $(编译器源码) yy_bs_stable yy豫构_stable | 运行时
	$(稳定豫构) 构建 豫言编译器 --编译器 $(稳定编译器) --输出 $@

yy3: $(编译器源码) yy2 yy豫构_stable
	./yy豫构_stable 构建 豫言编译器 --编译器 ./yy2 --输出 $@

yy4: $(编译器源码) yy3 yy豫构_stable
	./yy豫构_stable 构建 豫言编译器 --编译器 ./yy3 --输出 $@

连续自举: yy4_bs
	cmp yy3_bs yy4_bs

# 文言：以新编译器造豫构。汉语：工具独立运行，默认使用同目录的 yy4_bs 编译包。
豫构: yy豫构

# 文言：两种俱备，初举亦循包制。汉语：稳定编译器与稳定豫构由同一个发布包安装；禁止隐式单文件引导。
yy_bs_stable yy豫构_stable:
	@echo '缺少稳定种子；请先构建双种子发布包并安装 yy_bs_stable 与 yy豫构_stable。' >&2
	@exit 1

yy豫构: yy4_bs yy豫构_stable $(编译器源码)
	./yy豫构_stable 构建 豫构 --编译器 ./yy4_bs --输出 $@

豫构测试: yy豫构
	./yy豫构 构建 包配置 语言服务。测试 --输出 yy包语言服务测试
	./yy包语言服务测试
	./yy豫构 构建 豫构 包资料。测试 --输出 yy豫构资料测试
	./yy豫构资料测试
	./yy豫构 构建 豫构 配置解析。测试 --输出 yy豫构配置测试
	./yy豫构配置测试
	./yy豫构 构建 豫构 包系统。测试 --输出 yy豫构包测试
	./yy豫构包测试 ./yy豫构 ./yy4_bs
	./yy豫构 构建 豫构 调度器。测试 --输出 yy豫构调度测试
	./yy豫构调度测试
	./yy豫构 构建 豫构 原生构建。测试 --输出 yy豫构原生测试
	./yy豫构原生测试 ./yy豫构 ./yy4_bs
	./yy豫构 构建 豫构 并行进度。测试 --输出 yy豫构进度测试
	./yy豫构进度测试
	./yy豫构 构建 豫构 命令行。测试 --输出 yy豫构命令行测试
	./yy豫构命令行测试 ./yy豫构

自举包: yy豫构
	./yy豫构 自举包 --编译器 ./yy3_bs --输出 dist/yy-bootstrap

运行时 yy_runtime_lib:
	$(MAKE) -C 运行时支持库 全部 版本=$(版本)

清理生成数据:
	$(MAKE) -C 运行时支持库 清理
	rm -rf .yybuild .yybuild.nosync dist
	rm -f yy2_bs yy3_bs yy4_bs yy_parallel_deps.txt yy_parallel_log.txt

# 文言：工具入口以豫言行，C 惟桥宿主。汉语：Wasmtime C API 为显式本地依赖，不更改系统安装。
.PHONY: 网页汇编宿主
网页汇编宿主: yy网页汇编宿主

yy网页汇编宿主: 工具/网页汇编宿主/回收桥接.h 工具/网页汇编宿主/回收原语.h Makefile 工具/网页汇编宿主/入口。豫 工具/网页汇编宿主/宿主.c 运行时支持库/网页汇编/宿主协议.h 运行时支持库/原生/公共包含.h 运行时支持库/原生/值编解码.h 库/构建基础/包上下文.c yy_wasmtime_c_api/lib/libwasmtime.a
	clang -O2 -DNDEBUG -I 运行时支持库/原生 -I 运行时支持库/网页汇编 -I yy_wasmtime_c_api/include -c 工具/网页汇编宿主/宿主.c -o yy_网页宿主.o
	clang -O3 -DNDEBUG -I 运行时支持库/原生 -c 库/构建基础/包上下文.c -o yy_宿主包上下文.o
	llvm-ar rcs yy_网页宿主.a yy_网页宿主.o yy_宿主包上下文.o
	printf '%s\n' '豫构原生链接二' '$(CURDIR)/yy_网页宿主.a' '$(CURDIR)/yy_wasmtime_c_api/lib/libwasmtime.a' '-lpthread' '-lm' '-ldl' > yy_网页宿主链接清单
	./yy豫构 构建 网页汇编宿主 --编译器 $(网页汇编宿主编译器) --输出 $@ -j 4 -- --native-link-inputs '$(CURDIR)/yy_网页宿主链接清单'

# 文言：复制回收另立宿主，不混旧预编译物。汉语：同一 C 桥接链接显式采用 copying/128 MiB 的 Wasmtime 构建。
.PHONY: 网页汇编复制宿主
网页汇编复制宿主: yy网页汇编复制宿主

yy网页汇编复制宿主: yy网页汇编宿主 yy_wasmtime_gc_source/target/release/libwasmtime.a
	clang -O2 -DNDEBUG -DYY_WASM_GC_BULK -I 运行时支持库/原生 -I 运行时支持库/网页汇编 -I yy_wasmtime_c_api/include -c 工具/网页汇编宿主/宿主.c -o yy_网页批量宿主.o
	llvm-ar rcs yy_网页批量宿主.a yy_网页批量宿主.o yy_宿主包上下文.o
	printf '%s\n' '豫构原生链接二' '$(CURDIR)/yy_网页批量宿主.a' '$(CURDIR)/yy_wasmtime_gc_source/target/release/libwasmtime.a' '-lpthread' '-lm' '-ldl' > yy_网页复制宿主链接清单
	./yy豫构 构建 网页汇编宿主 --编译器 $(网页汇编宿主编译器) --输出 $@ -j 4 -- --native-link-inputs '$(CURDIR)/yy_网页复制宿主链接清单'
