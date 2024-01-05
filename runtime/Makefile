WASM_SOURCEDIR=$(abspath common) $(abspath wasm)
WASM_SOURCES := $(shell find $(WASM_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.ll' \) )
# WASM_SCFILES := $(shell find $(WASM_SOURCEDIR) \( -name '*.c'  -o -name '*.ll' \) )
WASM_SCFILES := $(shell find $(WASM_SOURCEDIR) \( -name '*.c'  -o -name '*.ll' \) )

# NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.ll' \) )

# WASM_SCPPFILES := $(shell find $(WASM_SOURCEDIR) \(  -name '*.cpp' \) )



NATIVE_SOURCEDIR=$(abspath common) $(abspath native)
NATIVE_SOURCES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h'  -o -name '*.ll' \) )
# NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.ll'  \) )
NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.ll' \) )
# NATIVE_SCPPFILES := $(shell find $(NATIVE_SOURCEDIR) \(  -name '*.cpp' \) )


CC=clang


UNAME := $(shell uname)

ADDITIONAL_INCLUDE_PATHS=

ifeq ($(UNAME), Linux)
ADDITIONAL_FLAGS=-l bsd
else
ADDITIONAL_FLAGS=
endif

# libyyrtdebug.a: $(NATIVE_SOURCES)
# 	make clean_object_files
# 	$(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
# 	# $(CC) -c -g $(NATIVE_SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
# 	ar rcs libyyrtdebug.a *.o 

# debug: libyyrtdebug.a

# libyyrtdebugwasm.a: $(WASM_SOURCES)
# 	make clean_object_files
# 	emcc -c -g $(WASM_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
# 	# emcc -c -g $(WASM_SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
# 	emar rcs libyyrtdebugwasm.a *.o

# wasmdebug: libyyrtdebugwasm.a

# libyyrtopt.a: $(NATIVE_SOURCES)
# 	make clean_object_files
# 	$(CC) -c -O3 $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
# 	# $(CC) -c -O3 $(NATIVE_SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
# 	ar rcs libyyrtopt.a *.o 

# opt : libyyrtopt.a


# clean_object_files:
# 	rm *.o

# clean:
# 	rm *.o libyyrt*.a

# libyyrtdebug.a: $(NATIVE_SOURCES)

debug: 
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include  -I $(abspath ../../bdwgc/include) $(ADDITIONAL_INCLUDE_PATHS) -Werror
	ar rcs $@ build/native/*.o

debug_old: 
	@mkdir -p build/native
	cd build/native && $(CC) -D OLD_ENTRY -c -g $(NATIVE_SCFILES) -I /usr/local/include  -I $(abspath ../../bdwgc/include) $(ADDITIONAL_INCLUDE_PATHS) -Werror
	ar rcs $@ build/native/*.o

# libyyrtdebugwasm.a: $(WASM_SOURCES)
# 	@mkdir -p build/wasm
# 	cd build/wasm && emcc -c -g $(WASM_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS)  -sMEMORY64
# 	emar rcs $@ build/wasm/*.o 

libyyrtdebugwasm.a: $(WASM_SOURCES)
	@mkdir -p build/wasm
	# cd build/wasm && $(CC) --target=wasm32-wasi -c -g $(WASM_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -I /usr/local/var/wasi-libc/sysroot/include
	# wasm-ld -o $@ build/wasm/*.o --no-entry
	cd build/wasm && emcc -c -g $(WASM_SCFILES) -I /usr/local/include -I ~/bdwgc/include -sMEMORY64
	emar rcs $@ build/wasm/*.o

wasmdebug: libyyrtdebugwasm.a

libyyrtopt.a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -O3 $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror
	ar rcs $@ build/native/*.o

opt : libyyrtopt.a

.PHONY: clean
clean:
	rm -rf build
	rm *.a