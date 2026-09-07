编译器入口 := 豫言编译器/入口。豫
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
