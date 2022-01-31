SOURCEDIR=files
SOURCES := $(shell find $(SOURCEDIR) -name '*.c')
CC=clang
libyyrtdebug.a: $(SOURCES)
	$(CC) -c -g $^ -I /usr/local/include 
	llvm-ar rcs libyyrtdebug.a *.o 

debug: libyyrtdebug.a

libyyrtopt.a: $(SOURCES)
	$(CC) -c -g $^ -I /usr/local/include
	llvm-ar rcs libyyrtopt.a *.o 

opt : libyyrtopt.a


clean:
	rm *.o libyyrt*.a