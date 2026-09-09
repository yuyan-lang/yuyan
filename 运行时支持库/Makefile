原生源码目录 := $(abspath 原生)
原生源码 := $(shell find $(原生源码目录) \( -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.ll' \))
原生源码 += Makefile
原生编译源码 := $(shell find $(原生源码目录) \( -name '*.c' -o -name '*.ll' \))
构建目录 := 构建/原生
原生目标文件 := $(addprefix $(构建目录)/,$(notdir $(原生编译源码)))
原生目标文件 := $(原生目标文件:.c=.o)
原生目标文件 := $(原生目标文件:.ll=.o)

编译器 := clang
系统名称 := $(shell uname)
版本 ?=

优化静态编译参数 := -flto
优化静态归档器 := llvm-ar

ifeq ($(系统名称),Linux)
优化静态编译参数 :=
优化静态归档器 := ar
endif

附加包含路径 := -I /opt/homebrew/include -I $(原生源码目录)

动态库构建参数 :=
ifeq ($(系统名称),Linux)
动态库构建参数 := -shared
else
动态库构建参数 := -dynamiclib -undefined dynamic_lookup
endif

动态库扩展名 :=
ifeq ($(系统名称),Linux)
动态库扩展名 := so
else
动态库扩展名 := dylib
endif

lib豫言运行时调试$(版本).a: $(原生源码)
	@mkdir -p $(构建目录)
	cd $(构建目录) && $(编译器) -c -g $(原生编译源码) -I /usr/local/include -I /opt/homebrew/include $(附加包含路径) -Wno-error
	rm -f $@
	ar rcs $@ $(原生目标文件)

调试静态库: lib豫言运行时调试$(版本).a

lib豫言运行时优化$(版本).a: $(原生源码)
	@mkdir -p $(构建目录)
	cd $(构建目录) && $(编译器) -DNDEBUG $(优化静态编译参数) -c -g -O3 $(原生编译源码) -I /usr/local/include -I /opt/homebrew/include $(附加包含路径) -Wno-error
	rm -f $@
	$(优化静态归档器) rcs $@ $(原生目标文件)

优化静态库: lib豫言运行时优化$(版本).a

lib豫言运行时调试$(版本).$(动态库扩展名): $(原生源码)
	@mkdir -p $(构建目录)
	cd $(构建目录) && $(编译器) -c -g $(原生编译源码) -I /usr/local/include -I /opt/homebrew/include $(附加包含路径) -Wno-error -fPIC
	$(编译器) $(动态库构建参数) -o $@ $(原生目标文件)

调试动态库: lib豫言运行时调试$(版本).$(动态库扩展名)

lib豫言运行时优化$(版本).$(动态库扩展名): $(原生源码)
	@mkdir -p $(构建目录)
	cd $(构建目录) && $(编译器) -DNDEBUG -flto -c -g -O3 $(原生编译源码) -I /usr/local/include -I /opt/homebrew/include $(附加包含路径) -Wno-error -fPIC
	$(编译器) $(动态库构建参数) -flto -o $@ $(原生目标文件)

优化动态库: lib豫言运行时优化$(版本).$(动态库扩展名)

全部: 调试静态库 优化静态库 调试动态库 优化动态库 包运行时静态库

# 文言：运行时唯备其核，包实现各归其籍。汉语：核心运行时不再编入包实现；保留包链接所用的稳定归档文件名。
lib豫言运行时包优化$(版本).a: lib豫言运行时优化$(版本).a
	cp $< $@.临时
	mv $@.临时 $@

lib豫言运行时包调试$(版本).a: lib豫言运行时调试$(版本).a
	cp $< $@.临时
	mv $@.临时 $@

.PHONY: 包运行时静态库
包运行时静态库: lib豫言运行时包优化$(版本).a lib豫言运行时包调试$(版本).a

.PHONY: 调试静态库 优化静态库 调试动态库 优化动态库 全部 清理
清理:
	rm -rf 构建
	rm -f lib豫言运行时*.a
	rm -f lib豫言运行时*.so
	rm -f lib豫言运行时*.dylib
	rm -f libyyrtopt*.a

# 因原生核心而适配网页汇编。WASI 首版静态库，仅收集可移植核心与显式替代接口。
网页汇编桥接参数 := $(if $(filter 1,$(YY_WASM_HOST_BRIDGE)),-DYY_WASM_HOST_BRIDGE,)
网页汇编初始堆 := $(if $(filter 1,$(YY_WASM_HOST_BRIDGE)),4194304,262144)
网页汇编工具链 ?= $(abspath ../yy_wasi_sdk)
网页汇编目录 := 构建/网页汇编
# 文言：真栈之跳返不入旧 WASI。汉语：直接调用的 setjmp 协议仅供原生目标，WasmGC 使用自身 EH。
网页汇编排除 := 子进程.c 传输控制协议.c 拉丁拼写.c 输入输出多路等待.c 直接调用.c
网页汇编核心 := $(filter-out $(addprefix $(原生源码目录)/,$(网页汇编排除)),$(原生编译源码))
网页汇编源码 := $(网页汇编核心) $(abspath 网页汇编/宿主适配.c) $(abspath 网页汇编/宿主进程.c)
网页汇编对象 := $(addprefix $(网页汇编目录)/,$(notdir $(网页汇编源码:.c=.o)))

.PHONY: 网页汇编库
网页汇编库:
	@mkdir -p $(网页汇编目录)
	cd $(网页汇编目录) && "$(网页汇编工具链)/bin/clang" --target=wasm32-wasip1 -mtail-call -O2 -DNDEBUG $(网页汇编桥接参数) -D初始堆大小=$(网页汇编初始堆) -I "$(原生源码目录)" -c $(网页汇编源码)
	rm -f $(网页汇编目录)/运行时.a
	"$(网页汇编工具链)/bin/llvm-ar" rcs $(网页汇编目录)/运行时.a $(网页汇编对象)
