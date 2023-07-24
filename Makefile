
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

bsrp : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --parse-only

bsrpvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --parse-only -vv

bsrtc : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --type-check-only

bsrtcvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --type-check-only -vv

bsrv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -v -o yy_bs_bs

bsrvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -vv -o yy_bs_bs

bsrvvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -vvv -o yy_bs_bs

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
	./yy_bs yylib/runtest.yuyan -- yy

bsrt_bs : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -- yy_bs

bsrtv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -v -- yy

bsrtvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vv -- yy

bsrtvvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vvv -- yy

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
	./yy_test_temp yy

test_bs: build yy_bs yy_test_temp
	./yy_test_temp yy_bs
	
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

cleancache:
	find .yybuild.nosync/ -name "*.编译信息.json" -print -exec rm {} \;

cleanallcache:
	find .yybuild.nosync/ -name "*.json" -print -exec rm {} \;


superclean:
	rm -f yy
	rm -rf ./.yybuild.nosync/*

MODULE_NAME = $(error Please set MODULE_NAME as a command line argument for locating target-specific files)
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
