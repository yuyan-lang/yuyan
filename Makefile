
SMLSOURCEDIR=src
YYBSSOURCEDIR=豫言编译器
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")
YYBSSOURCES := $(shell find $(YYBSSOURCEDIR) -name '*。豫' ! -path "*/.cm/*")
YYTESTSOURCES := $(shell find . -name '*。测试。豫' ! -path "*/.cm/*")

build: yy yyrt

yyrt:
	make -C runtime/ opt
	make -C runtime/ debug

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 2 src/development.mlb

yy_bs : yy $(YYBSSOURCES) 
	./yy -c --use-local-lib 豫言编译器/入口。豫  -o yy_bs

bsrtc : yy_bs 
	make yyrt
	./yy_bs 豫言编译器/入口。豫 --type-check-only

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
	./yy_bs yylib/runtest.yuyan

bsrtv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -v

bsrtvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vv

bsrtvvv : yy_bs $(YYTESTSOURCES)
	./yy_bs yylib/runtest.yuyan -vvv

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

test: build
	./yy yylib/runtest.yuyan --use-local-lib -c -o ./yy_test_temp
	./yy_test_temp yy
	
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


superclean:
	rm -f yy
	rm -rf ./.yybuild.nosync/*

wasm: 
	rm -rf wasmbuild
	mkdir -p wasmbuild
	mlton -output ./wasmbuild/yywasmll -stop g -codegen llvm  -keep g -verbose 2 src/development.mlb
	emcc wasmbuild/*.ll wasmbuild/*.c -L/usr/local/lib/mlton/targets/self -lmlton -lgdtoa -lm -lgmp -L/usr/local/opt/gmp/lib  -I/usr/local/lib/mlton/targets/self/include -I/usr/local/lib/mlton/include -I/usr/local/opt/gmp/include  -std=gnu11