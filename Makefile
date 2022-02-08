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
	cp runtime/libyyrtdebug.a /usr/local/lib/libyuyanlangruntimedebug.a
	cp runtime/libyyrtopt.a /usr/local/lib/libyuyanlangruntime.a
	cp yy /usr/local/bin/yuyanlang
	ln -sf /usr/local/bin/yuyanlang /usr/local/bin/yy

installtest:  buildtest
	cp yy_test /usr/local/bin/yy_test

clean:
	rm yy

superclean:
	rm -f yy
	rm -rf ./.yybuild/*
