SMLSOURCEDIR=src
SMLSOURCES := $(shell find $(SMLSOURCEDIR) -name '*.sml' ! -path "*/.cm/*")

build: yy

yy:  $(SMLSOURCES)
	mlton -output yy -verbose 1 src/development.mlb

test: build
	./yy rv yylib/runtest.yuyan
	


clean:
	rm yy

superclean:
	rm -f yy
	rm -rf ./.yybuild/*