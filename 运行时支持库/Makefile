原生源码目录 := $(abspath 原生)
原生源码 := $(shell find $(原生源码目录) \( -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.ll' \))
自举桥接源码 := $(原生源码目录)/自举桥接.c
原生编译源码 := $(filter-out $(自举桥接源码),$(shell find $(原生源码目录) \( -name '*.c' -o -name '*.ll' \)))
构建目录 := 构建/原生
原生目标文件 := $(addprefix $(构建目录)/,$(notdir $(原生编译源码)))
原生目标文件 := $(原生目标文件:.c=.o)
原生目标文件 := $(原生目标文件:.ll=.o)
自举桥接目标文件 := $(构建目录)/自举桥接.o

编译器 := clang
系统名称 := $(shell uname)
版本 ?=

优化静态编译参数 := -flto
优化静态归档器 := llvm-ar

ifeq ($(系统名称),Linux)
优化静态编译参数 :=
优化静态归档器 := ar
endif

附加包含路径 := -I /opt/homebrew/include

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

# 仅供现有稳定编译器完成第一级自举；新编译器直接链接上面的中文库名。
libyyrtopt$(版本).a: lib豫言运行时优化$(版本).a $(自举桥接源码)
	@mkdir -p $(构建目录)
	cd $(构建目录) && $(编译器) -DNDEBUG $(优化静态编译参数) -c -g -O3 $(自举桥接源码) -I /usr/local/include -I /opt/homebrew/include $(附加包含路径) -Wno-error -o 自举桥接.o
	cp -f $< $@
	$(优化静态归档器) rcs $@ $(自举桥接目标文件)

首级自举运行时库: libyyrtopt$(版本).a

全部: 调试静态库 优化静态库 调试动态库 优化动态库 首级自举运行时库

.PHONY: 调试静态库 优化静态库 调试动态库 优化动态库 首级自举运行时库 全部 清理
清理:
	rm -rf 构建
	rm -f lib豫言运行时*.a
	rm -f lib豫言运行时*.so
	rm -f lib豫言运行时*.dylib
	rm -f libyyrtopt*.a

# 因原生核心而适配网页汇编。WASI 首版静态库，仅收集可移植核心与显式替代接口。
网页汇编工具链 ?= $(abspath ../yy_wasi_sdk)
网页汇编目录 := 构建/网页汇编
网页汇编排除 := 自举桥接.c 子进程.c 传输控制协议.c 拉丁拼写.c 输入输出多路等待.c
网页汇编核心 := $(filter-out $(addprefix $(原生源码目录)/,$(网页汇编排除)),$(原生编译源码))
网页汇编源码 := $(网页汇编核心) $(abspath 网页汇编/宿主适配.c)
网页汇编对象 := $(addprefix $(网页汇编目录)/,$(notdir $(网页汇编源码:.c=.o)))

.PHONY: 网页汇编库
网页汇编库:
	@mkdir -p $(网页汇编目录)
	cd $(网页汇编目录) && "$(网页汇编工具链)/bin/clang" --target=wasm32-wasip1 -mtail-call -O2 -DNDEBUG -D初始堆大小=262144 -I "$(原生源码目录)" -c $(网页汇编源码)
	rm -f $(网页汇编目录)/运行时.a
	"$(网页汇编工具链)/bin/llvm-ar" rcs $(网页汇编目录)/运行时.a $(网页汇编对象)
