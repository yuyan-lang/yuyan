SMLSOURCEDIR=src
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")

build: yy

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 1 src/development.mlb

test: build
	./yy rv yylib/runtest.yuyan
	
genDocs : build
	./yy genDocs yylib/总库。豫 

install:  build
	cp yy /usr/local/bin/yuyanlang
	ln -sf /usr/local/bin/yuyanlang /usr/local/bin/yy

clean:
	rm yy

superclean:
	rm -f yy
	rm -rf ./.yybuild/*
