

NATIVE_SOURCEDIR= $(abspath native)
NATIVE_SOURCES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h'  -o -name '*.ll' \) )
NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.ll' \) )


CC=clang


UNAME := $(shell uname)

ADDITIONAL_INCLUDE_PATHS=

ifeq ($(UNAME), Linux)
ADDITIONAL_FLAGS=-l bsd
else
ADDITIONAL_FLAGS=
endif


libyyrtdebug.a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include  -I $(abspath ../../bdwgc/include) $(ADDITIONAL_INCLUDE_PATHS) -Werror
	ar rcs $@ build/native/*.o

debug: libyyrtdebug.a

libyyrtopt.a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -DNDEBUG -flto -c -g -O3 $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror
	ar rcs $@ build/native/*.o

opt : libyyrtopt.a

libyyrtdebug.so: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror  -fPIC
	$(CC) -shared -o $@ build/native/*.o

debugso : libyyrtdebug.so

libyyrtopt.so: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -DNDEBUG -flto -c -g -O3 $(NATIVE_SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -fPIC
	$(CC) -shared -flto -o $@ build/native/*.o

optso : libyyrtopt.so

all: debug opt debugso optso

.PHONY: clean
clean:
	rm -rf build
	rm *.a