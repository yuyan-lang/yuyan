WASM_SOURCEDIR=common wasm
WASM_SOURCES := $(shell find $(WASM_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h' \) )
WASM_SCFILES := $(shell find $(WASM_SOURCEDIR) \( -name '*.c'  \) )
WASM_SCPPFILES := $(shell find $(WASM_SOURCEDIR) \(  -name '*.cpp' \) )



NATIVE_SOURCEDIR=common native
NATIVE_SOURCES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h' \) )
NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c'  \) )
NATIVE_SCPPFILES := $(shell find $(NATIVE_SOURCEDIR) \(  -name '*.cpp' \) )


CC=clang


UNAME := $(shell uname)

ADDITIONAL_INCLUDE_PATHS=

ifeq ($(UNAME), Linux)
ADDITIONAL_FLAGS=-l bsd
else
ADDITIONAL_FLAGS=
endif

libyyrtdebug.a: $(NATIVE_SOURCES)
	$(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
	$(CC) -c -g $(NATIVE_SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
	ar rcs libyyrtdebug.a *.o 

debug: libyyrtdebug.a

libyyrtdebugwasm.a: $(WASM_SOURCES)
	emcc -c -g $(WASM_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
	emcc -c -g $(WASM_SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
	emar rcs libyyrtdebugwasm.a *.o

wasmdebug: libyyrtdebugwasm.a

libyyrtopt.a: $(NATIVE_SOURCES)
	$(CC) -c -O3 $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
	$(CC) -c -O3 $(NATIVE_SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
	ar rcs libyyrtopt.a *.o 

opt : libyyrtopt.a


clean:
	rm *.o libyyrt*.a
