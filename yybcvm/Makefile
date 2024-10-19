

NATIVE_SOURCEDIR= $(abspath native)
NATIVE_SOURCES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' \) )
NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.ll' \) )
VM_SOURCEDIR := $(abspath vm)
VM_CFILES := $(shell find $(VM_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' \) )
PYGEN_SOURCEDIR := $(abspath pygen)
PYGEN_CFILES := $(shell find $(PYGEN_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' \) )


CC=clang


UNAME := $(shell uname)

ADDITIONAL_INCLUDE_PATHS=

ifeq ($(UNAME), Linux)
ADDITIONAL_FLAGS=-l bsd
else
ADDITIONAL_FLAGS=
endif

SO_BUILD_OPTIONS=

ifeq ($(UNAME), Linux)
SO_BUILD_OPTIONS=-shared 
else
SO_BUILD_OPTIONS=-dynamiclib -undefined dynamic_lookup
endif


SO_EXTENSION=
ifeq ($(UNAME), Linux)
SO_EXTENSION=so
else
SO_EXTENSION=dylib
endif

allvm:  vm vmopt

vm: $(NATIVE_SOURCES) $(VM_CFILES)
	@mkdir -p build/native
	$(CC) -g $^ -I /usr/local/include -I /opt/homebrew/include -L /usr/local/lib -L /opt/homebrew/lib -luv -lgc $(ADDITIONAL_INCLUDE_PATHS) -Werror -o build/native/vm.exe


vmopt: $(NATIVE_SOURCES) $(VM_CFILES)
	$(CC) -g $^ -O3 -DNDEBUG -I /usr/local/include -I /opt/homebrew/include -L /usr/local/lib -L /opt/homebrew/lib -luv -lgc $(ADDITIONAL_INCLUDE_PATHS) -Werror -o build/native/vmopt.exe
	
PYGEN_EXEC_PATH=$(error Please specify PYGEN_EXEC_PATH to compile pygen)
PYGEN_OPT_EXEC_PATH=$(PYGEN_EXEC_PATH:.exe=.opt.exe)
pygen: $(NATIVE_SOURCES) $(PYGEN_CFILES)
	$(CC) -g $^ -I /usr/local/include -I /opt/homebrew/include -L /usr/local/lib -L /opt/homebrew/lib -luv -lgc $(ADDITIONAL_INCLUDE_PATHS) -Werror -o $(PYGEN_EXEC_PATH)
	$(CC) -g -O3 -DNDEBUG $^ -I /usr/local/include -I /opt/homebrew/include -L /usr/local/lib -L /opt/homebrew/lib -luv -lgc $(ADDITIONAL_INCLUDE_PATHS) -Werror -o $(PYGEN_OPT_EXEC_PATH)



libyyrtdebug$(VERSION).a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include  -I /opt/homebrew/include -I $(abspath ../../bdwgc/include) $(ADDITIONAL_INCLUDE_PATHS) -Werror
	ar rcs $@ build/native/*.o

debug: libyyrtdebug$(VERSION).a

libyyrtopt$(VERSION).a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -DNDEBUG -flto -c -g -O3 $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include $(ADDITIONAL_INCLUDE_PATHS) -Werror
	llvm-ar rcs $@ build/native/*.o

opt : libyyrtopt$(VERSION).a

libyyrtdebug$(VERSION).$(SO_EXTENSION): $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include $(ADDITIONAL_INCLUDE_PATHS) -Werror  -fPIC
	$(CC) $(SO_BUILD_OPTIONS) -o $@ build/native/*.o

debugso : libyyrtdebug$(VERSION).$(SO_EXTENSION)

libyyrtopt$(VERSION).$(SO_EXTENSION): $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -DNDEBUG -flto -c -g -O3 $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include/ $(ADDITIONAL_INCLUDE_PATHS) -Werror -fPIC
	$(CC) $(SO_BUILD_OPTIONS) -flto -o $@ build/native/*.o

optso : libyyrtopt$(VERSION).$(SO_EXTENSION)

all: debug opt debugso optso

.PHONY: clean
clean:
	rm -rf build
	rm -f *.a
	rm -f *.so
