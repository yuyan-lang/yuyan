SOURCEDIR=files
SOURCES := $(shell find $(SOURCEDIR) -name '*.c')
CC=clang


UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ADDITIONAL_FLAGS=-l bsd
else
ADDITIONAL_FLAGS=
endif

libyyrtdebug.a: $(SOURCES)
	$(CC) -c -g $^ -I /usr/local/include 
	llvm-ar rcs libyyrtdebug.a *.o 

debug: libyyrtdebug.a

libyyrtopt.a: $(SOURCES)
	$(CC) -c -g $^ -I /usr/local/include
	llvm-ar rcs libyyrtopt.a *.o 

opt : libyyrtopt.a


executable: $(LL_FILES) $(RT_LIB_PATH) 
	clang $^ -save-temps=obj $(CC_FLAGS) -o $(OUTPUT_PATH) -L /usr/local/lib -l gc -l uv $(ADDITIONAL_FLAGS) -Wno-override-module

clean:
	rm *.o libyyrt*.a