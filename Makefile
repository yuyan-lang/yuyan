
SMLSOURCEDIR=src
YYBSSOURCEDIR=豫言编译器
YYLIBSOURCEDIR=yylib
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")
YYBSSOURCES := $(shell find $(YYBSSOURCEDIR) -name '*。豫' ! -path "*/.cm/*")
YYLIBSOURCES := $(shell find $(YYLIBSOURCEDIR) -name '*。豫' ! -path "*/.cm/*")
YYTESTSOURCES := $(shell find . -name '*。测试。豫' ! -path "*/.cm/*")

build: yy yyrt

yyrt:
	# make -C runtime/ opt
	make -C runtime/ debug

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 2 src/development.mlb

yy_bs : yy $(YYBSSOURCES) $(YYLIBSOURCES)
	./yy -c --use-local-lib 豫言编译器/入口。豫  -o yy_bs

yy_bs_bs : $(YYBSSOURCES) $(YYLIBSOURCES) 
	./yy_bs 豫言编译器/入口。豫  -o yy_bs_bs --parallel

bsrp : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --parse-only

bsrtc : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --type-check-only

bsr : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -o yy_bs_bs

bsrjs : yy_bs 
	./yy_bs 豫言编译器/入口。豫 -o yy_bs_bs --target=js

bsrwhole : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -o yy_bs_bs --whole-program-opt

bst : yy_bs $(YYTESTSOURCES)
	./yy_bs $(YYTESTSOURCES)

bsrt : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -- ./yy

bsrt_bs : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -- ./yy_bs

bsrtv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -v -- ./yy

bsrtvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vv -- ./yy

bsrtvvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vvv -- ./yy

bsrttc : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan --type-check-only

bsrttcv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -v --type-check-only

bsrttcvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vv --type-check-only

bsrttcvvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vvv --type-check-only

yyt : yy $(YYTESTSOURCES)
	./yy $(YYTESTSOURCES)

yyllvm:  $(SMLSOURCES)
	mlton -codegen llvm -output yy -verbose 2 src/development.mlb

buildtest:
	./yy -c yylib/runtest.yuyan -o yy_test

yy_test_temp: build
	./yy yylib/runtest.yuyan --use-local-lib -c -o ./yy_test_temp

test: build yy_test_temp yy
	./yy_test_temp ./yy --use-local-lib

test_bs: build yy_bs yy_test_temp
	./yy_test_temp ./yy_bs

test_bs_js: build yy_bs yy_test_temp
	./yy_test_temp ./yy_bs --target=js

test_bs_bs: build yy_test_temp
	./yy_test_temp ./yy_bs_bs
	
genDocs : build
	rm -rf ./.yybuild.nosync/docs
	./yy --gen-docs yylib/总库。豫 

installDocs : genDocs
	rm -rf ./docs/autogen/docs
	cp -r ./.yybuild.nosync/docs ./docs/autogen/

install:  build
	sh -x install.sh

clean:
	rm yy

cleanbs:
	rm yy_bs

cleancodegen:
	find .yybuild.nosync/ -name "*.代码生成形式.json" -print -exec rm {} \;

cleanclosureopt:
	find .yybuild.nosync/ -name "*.代码生成形式.json" -print -exec rm {} \;
	find .yybuild.nosync/ -name "*.闭包优化后形式.json" -print -exec rm {} \;


superclean:
	rm -f yy
	rm -rf ./.yybuild.nosync/*

MODULE_NAME = $(error Please set MODULE_NAME as a command line argument for locating target-specific files)

cleansinglefile:
	find .yybuild.nosync/ -name "$(MODULE_NAME).*.json" -print -exec rm {} \;

wasm: yyrt
	make -C runtime/ wasmdebug
	llvm-dis ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.bc -o ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.ll
	emcc -o yy_$(MODULE_NAME)_test.js -O3 ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.ll ./runtime/libyyrtdebugwasm.a -L /usr/local/lib -l stdc++ -Wno-override-module -g -mtail-call -sMEMORY64

emwasm: yyrt
	make -C runtime/ clean
	make -C runtime/ emwasmdebug
	llvm-dis ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.bc -o ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.ll
	emcc -o yy_$(MODULE_NAME)_test.js -O3 ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.ll ./runtime/libyyrtdebugemwasm.a ~/bdwgc/emwasmout/libgc.a -L /usr/local/lib -l stdc++  -Wno-override-module -g3 -mtail-call -Wbad-function-cast -Wcast-function-type -O0

debugll:
	llvm-dis ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.bc -o ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.ll
	python3 unescape.py ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.ll
	llvm-dis ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.opt.bc -o ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.opt.ll
	python3 unescape.py ./.yybuild.nosync/yy_$(MODULE_NAME)_豫言编译器默认执行包.opt.ll

USER_CODE_DIR = $(error Please set USER_CODE_DIR as a command line argument in server.js when serving the compilation for wasm/emwasm)
compile_user_code:
	make -C runtime/ wasmdebug
	./yy_bs_bs $(USER_CODE_DIR)/user-code.yuyan
	cp ./.yybuild.nosync/yy_user-code_豫言编译器默认执行包.bc ./.yybuild.nosync/$(USER_CODE_DIR)/yy_user-code_豫言编译器默认执行包.bc
	llvm-dis ./.yybuild.nosync/$(USER_CODE_DIR)/yy_user-code_豫言编译器默认执行包.bc -o ./.yybuild.nosync/$(USER_CODE_DIR)/yy_user-code_豫言编译器默认执行包.ll
	emcc -o ./.yybuild.nosync/$(USER_CODE_DIR)/user-code-compiled.js -O3 ./.yybuild.nosync/$(USER_CODE_DIR)/yy_user-code_豫言编译器默认执行包.ll ./runtime/libyyrtdebugwasm.a -L /usr/local/lib -l stdc++ -Wno-override-module -g -mtail-call -sMEMORY64 --pre-js ide/client/ide-pre.js


createcache:
	mkdir -p .yybuild.nosync
	mount -t tmpfs none .yybuild.nosync

cleancache:
	find .yybuild.nosync/  -type f -delete 
	find .yybuild.nosync/ -mindepth 1 -type d -delete 

CACHE_DIR := ./.yybuild.nosync

.PHONY: make backupcache restorecache

backupcache:
	echo "Creating backup..."
	# mkdir -p yy_backup_temp
	# cp -r $(CACHE_DIR)/* yy_backup_temp
	# rm -f yy_cache_data.zip
	(cd .yybuild.nosync/ && zip -q -r ../yy_cache_data_2.zip .)
	mv yy_cache_data_2.zip yy_cache_data.zip
	# rm -rf yy_backup_temp
	echo "Backup completed. Zip file: yy_cache_data.zip"

restorecache:
	echo "Restoring cache..."
	unzip -q yy_cache_data.zip -d yy_restore_temp
	cp -n -r yy_restore_temp/* $(CACHE_DIR)
	rm -rf yy_restore_temp
	echo "Cache restored."