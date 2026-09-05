编译器入口 := 豫言编译器/入口。豫
稳定编译器 := ./yy_bs_stable
编译器源码 := $(shell find 豫言编译器 藏书阁 工具/豫构 -type f \( -name '*。豫' -o -name '*.豫' -o -name '*.yuyan' \))
版本 := $(shell sed -n -E 's/^.*v(.*)\+([0-9]{4}).*/v\1+\2/p' 豫言编译器/编译辅助工具/命令行/版本管理。豫)

.PHONY: 全部 类型检查 连续自举 自举包 运行时 yy_runtime_lib 清理生成数据

全部: 连续自举

类型检查:
	$(稳定编译器) $(编译器入口) --type-check-only

yy2_bs: $(编译器源码) yy_bs_stable
	$(稳定编译器) $(编译器入口) -o $@

yy3_bs: $(编译器源码) yy2_bs
	./yy2_bs $(编译器入口) -o $@ --parallel

yy4_bs: $(编译器源码) yy3_bs
	./yy3_bs $(编译器入口) -o $@ --parallel

yy2: $(编译器源码) yy_bs_stable
	$(稳定编译器) $(编译器入口) -o $@

yy3: $(编译器源码) yy2
	./yy2 $(编译器入口) -o $@ --parallel

yy4: $(编译器源码) yy3
	./yy3 $(编译器入口) -o $@ --parallel

连续自举: yy4_bs
	cmp yy3_bs yy4_bs

自举包: yy3_bs
	./yy3_bs $(编译器入口) --emit-bootstrap-bundle dist/yy-bootstrap --parallel

运行时 yy_runtime_lib:
	$(MAKE) -C 运行时支持库 全部 版本=$(版本)

清理生成数据:
	$(MAKE) -C 运行时支持库 清理
	rm -rf .yybuild .yybuild.nosync dist
	rm -f yy2_bs yy3_bs yy4_bs yy_parallel_deps.txt yy_parallel_log.txt
