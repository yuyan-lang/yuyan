SMLSOURCEDIR=src
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")

build: yy yyrt

yyrt:
	make -C runtime/ opt
	make -C runtime/ debug

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 1 src/development.mlb

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