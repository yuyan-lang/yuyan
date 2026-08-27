

NATIVE_SOURCEDIR= $(abspath native)
NATIVE_SOURCES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h'  -o -name '*.ll' \) )
NATIVE_SCFILES := $(shell find $(NATIVE_SOURCEDIR) \( -name '*.c' -o -name '*.ll' \) )
NATIVE_OBJECTS := $(addprefix build/native/,$(notdir $(NATIVE_SCFILES)))
NATIVE_OBJECTS := $(NATIVE_OBJECTS:.c=.o)
NATIVE_OBJECTS := $(NATIVE_OBJECTS:.ll=.o)


CC=clang


UNAME := $(shell uname)

ADDITIONAL_INCLUDE_PATHS=-I /opt/homebrew/include

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


libyyrtdebug$(VERSION).a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include $(ADDITIONAL_INCLUDE_PATHS) -Wno-error
	$(RM) $@
	ar rcs $@ $(NATIVE_OBJECTS)

debug: libyyrtdebug$(VERSION).a

libyyrtopt$(VERSION).a: $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -DNDEBUG -flto -c -g -O3 $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include $(ADDITIONAL_INCLUDE_PATHS) -Wno-error
	$(RM) $@
	ar rcs $@ $(NATIVE_OBJECTS)

opt : libyyrtopt$(VERSION).a

libyyrtdebug$(VERSION).$(SO_EXTENSION): $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -c -g $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include $(ADDITIONAL_INCLUDE_PATHS) -Wno-error -fPIC
	$(CC) $(SO_BUILD_OPTIONS) -o $@ $(NATIVE_OBJECTS)

debugso : libyyrtdebug$(VERSION).$(SO_EXTENSION)

libyyrtopt$(VERSION).$(SO_EXTENSION): $(NATIVE_SOURCES)
	@mkdir -p build/native
	cd build/native && $(CC) -DNDEBUG -flto -c -g -O3 $(NATIVE_SCFILES) -I /usr/local/include -I /opt/homebrew/include $(ADDITIONAL_INCLUDE_PATHS) -Wno-error -fPIC
	$(CC) $(SO_BUILD_OPTIONS) -flto -o $@ $(NATIVE_OBJECTS)

optso : libyyrtopt$(VERSION).$(SO_EXTENSION)

all: debug opt debugso optso

.PHONY: clean
clean:
	rm -rf build
	rm -f *.a
	rm -f *.so
	rm -f *.dylib
