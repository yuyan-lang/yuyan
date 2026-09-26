原生源码目录 := $(abspath ../豫言操作系统/宿主/原生/运行时)
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

# 文言：编译器恒以静态链接，动态库无所用。汉语：编译器总是静态链接运行时，不再构建动态库。
全部: 调试静态库 优化静态库 包运行时静态库

# 文言：运行时唯备其核，包实现各归其籍。汉语：核心运行时不再编入包实现；保留包链接所用的稳定归档文件名。
lib豫言运行时包优化$(版本).a: lib豫言运行时优化$(版本).a
	cp $< $@.临时
	mv $@.临时 $@

lib豫言运行时包调试$(版本).a: lib豫言运行时调试$(版本).a
	cp $< $@.临时
	mv $@.临时 $@

.PHONY: 包运行时静态库
包运行时静态库: lib豫言运行时包优化$(版本).a lib豫言运行时包调试$(版本).a

.PHONY: 调试静态库 优化静态库 全部 清理
清理:
	rm -rf 构建
	rm -f lib豫言运行时*.a
	rm -f lib豫言运行时*.so
	rm -f lib豫言运行时*.dylib
	rm -f libyyrtopt*.a
