SOURCEDIR=files
SOURCES := $(shell find $(SOURCEDIR) \( -name '*.c' -o -name '*.cpp' -o -name '*.h' \) )
SCFILES := $(shell find $(SOURCEDIR) \( -name '*.c'  \) )
SCPPFILES := $(shell find $(SOURCEDIR) \(  -name '*.cpp' \) )


CC=clang


UNAME := $(shell uname)

ADDITIONAL_INCLUDE_PATHS=

ifeq ($(UNAME), Linux)
ADDITIONAL_FLAGS=-l bsd
else
ADDITIONAL_FLAGS=
endif

libyyrtdebug.a: $(SOURCES)
	$(CC) -c -g $(SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
	$(CC) -c -g $(SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
	ar rcs libyyrtdebug.a *.o 

debug: libyyrtdebug.a

libyyrtopt.a: $(SOURCES)
	$(CC) -c -O3 $(SCFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror 
	$(CC) -c -O3 $(SCPPFILES) -I /usr/local/include $(ADDITIONAL_INCLUDE_PATHS) -Werror -std=c++17
	ar rcs libyyrtopt.a *.o 

opt : libyyrtopt.a


clean:
	rm *.o libyyrt*.a