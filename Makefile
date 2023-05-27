
SMLSOURCEDIR=src
YYBSSOURCEDIR=豫言编译器
YYLIBSOURCEDIR=yylib
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")
YYBSSOURCES := $(shell find $(YYBSSOURCEDIR) -name '*。豫' ! -path "*/.cm/*")
YYLIBSOURCES := $(shell find $(YYLIBSOURCEDIR) -name '*。豫' ! -path "*/.cm/*")
YYTESTSOURCES := $(shell find . -name '*。测试。豫' ! -path "*/.cm/*")

build: yy yyrt

yyrt:
	make -C runtime/ opt
	make -C runtime/ debug

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 2 src/development.mlb

yy_bs : yy $(YYBSSOURCES) $(YYLIBSOURCES)
	./yy -c --use-local-lib 豫言编译器/入口。豫  -o yy_bs

bsrtc : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --type-check-only

bsrtcvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --type-check-only -vv

bsrv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -v

bsrvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -vv

bsrvvv : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 -vvv

bsr : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫
	# ./yy_bs 豫言编译器/编译步骤/语法分析/词法解析。豫
	# ./yy_bs tests/example/example-3.yuyan
	# ./yy_bs tests/comments/nested-1.yuyan
	# ./yy_bs yylib/标准库。豫
	# ./yy_bs tests/syntax/import/hello-world-2.yuyan

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
	rm -rf .yybuild.nosync/yylib
	rm -rf .yybuild.nosync/tests
	rm -rf .yybuild.nosync/豫言编译器


superclean:
	rm -f yy
	rm -rf ./.yybuild.nosync/*

wasm: yyrt
	make -C runtime/ wasmdebug
	llvm-dis ./.yybuild.nosync/豫言编译器默认执行包.bc -o ./.yybuild.nosync/豫言编译器默认执行包.ll
	emcc -o test.html -O3 ./.yybuild.nosync/豫言编译器默认执行包.ll ./runtime/libyyrtdebugwasm.a -L /usr/local/lib -l gc -l uv -l stdc++ -Wno-override-module -g -mtail-call

debugll:
	llvm-dis ./.yybuild.nosync/豫言编译器默认执行包.bc -o ./.yybuild.nosync/豫言编译器默认执行包.ll
	python3 unescape.py ./.yybuild.nosync/豫言编译器默认执行包.ll
	llvm-dis ./.yybuild.nosync/豫言编译器默认执行包.opt.bc -o ./.yybuild.nosync/豫言编译器默认执行包.opt.ll
	python3 unescape.py ./.yybuild.nosync/豫言编译器默认执行包.opt.ll
