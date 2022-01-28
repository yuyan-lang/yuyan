SOURCEDIR=files
SOURCES := $(shell find $(SOURCEDIR) -name '*.c')
CC=clang
# files/libuv/filesystem.c files/libuv/processes.c files/allocation.c files/entry.c files/exception.c files/io.c files/marshall.c 
libyyrtdebug.a: $(SOURCES)
	$(CC) -c -g $^ -I /usr/local/Cellar/bdw-gc/8.0.6/include  -I /usr/local/Cellar/libuv/1.42.0/include 
	llvm-ar rcs libyyrtdebug.a *.o 

debug: libyyrtdebug.a

libyyrtopt.a: $(SOURCES)
	$(CC) -c -g $^ -I /usr/local/Cellar/bdw-gc/8.0.6/include  -I /usr/local/Cellar/libuv/1.42.0/include  -O3
	llvm-ar rcs libyyrtopt.a *.o 

opt : libyyrtopt.a


clean:
	rm *.o libyyrt*.a