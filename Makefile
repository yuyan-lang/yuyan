SMLSOURCEDIR=src
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")

build: yy yyrt

yyrt:
	make -C runtime/ opt
	make -C runtime/ debug

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 1 src/development.mlb

test: build
	./yy yylib/runtest.yuyan --use-local-lib
	
genDocs : build
	./ genDocs yylib/总库。豫 

install:  build
	cp runtime/libyyrtdebug.a /usr/local/lib/libyuyanlangruntimedebug.a
	cp runtime/libyyrtopt.a /usr/local/lib/libyuyanlangruntime.a
	cp yy /usr/local/bin/yuyanlang
	ln -sf /usr/local/bin/yuyanlang /usr/local/bin/yy

clean:
	rm yy

superclean:
	rm -f yy
	rm -rf ./.yybuild/*
