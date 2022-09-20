.PHONY: srcyy

SMLSOURCEDIR=src
YYBSSOURCEDIR=srcyy
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")
YYBSSOURCES := $(shell find $(YYBSSOURCEDIR) -name '*。豫' ! -path "*/.cm/*")

build: yy yyrt

yyrt:
	make -C runtime/ opt
	make -C runtime/ debug

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 2 src/development.mlb

yy_bs : yy $(YYBSSOURCES) build
	./yy -c --use-local-lib srcyy/入口。豫  -o yy_bs

bsr : yy_bs 
	# ./yy_bs srcyy/入口。豫
	./yy_bs srcyy/编译步骤/语法分析/移位规约解析。豫

yyllvm:  $(SMLSOURCES)
	mlton -codegen llvm -output yy -verbose 2 src/development.mlb

buildtest:
	./yy -c yylib/runtest.yuyan -o yy_test

test: build
	./yy yylib/runtest.yuyan --use-local-lib
	
genDocs : build
	rm -rf ./.yybuild/docs
	./yy --gen-docs yylib/总库。豫 

installDocs : genDocs
	rm -rf ./docs/autogen/docs
	cp -r ./.yybuild/docs ./docs/autogen/

install:  build
	sh -x install.sh

clean:
	rm yy

superclean:
	rm -f yy
	rm -rf ./.yybuild/*

wasm: 
	rm -rf wasmbuild
	mkdir -p wasmbuild
	mlton -output ./wasmbuild/yywasmll -stop g -codegen llvm  -keep g -verbose 2 src/development.mlb
	emcc wasmbuild/*.ll wasmbuild/*.c -L/usr/local/lib/mlton/targets/self -lmlton -lgdtoa -lm -lgmp -L/usr/local/opt/gmp/lib  -I/usr/local/lib/mlton/targets/self/include -I/usr/local/lib/mlton/include -I/usr/local/opt/gmp/include  -std=gnu11