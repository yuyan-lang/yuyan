

libyyrt.a: files/libuv/filesystem.c files/libuv/processes.c files/allocation.c files/entry.c files/exception.c files/io.c files/marshall.c
	clang -c -g $^ -I /usr/local/Cellar/bdw-gc/8.0.6/include -L /usr/local/Cellar/bdw-gc/8.0.6/lib -l gc -I /usr/local/Cellar/libuv/1.42.0/include -L /usr/local/Cellar/libuv/1.42.0/lib -l uv
	llvm-ar rcs libyyrt.a *.o 

clean:
	rm *.o libyyrt.a