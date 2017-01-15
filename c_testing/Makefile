LIBRARIES=cairo pangocairo
RELEASE?=1
DEVCFLAGS = -g
RELEASECFLAGS=-O2 -DNDEBUG
ifeq ($(RELEASE), 1)
	BUILDTYPECFLAGS=$(RELEASECFLAGS)
else
	BUILDTYPECFLAGS=$(DEVCFLAGS)
endif
CFLAGS = $(BUILDTYPECFLAGS) -Wall -Wextra -Isrc `pkg-config --cflags $(LIBRARIES)` $(OPTFLAGS)
LDFLAGS=`pkg-config --libs $(LIBRARIES)`
SRCDIR=src
OBJDIR=build
BINDIR=bin

SOURCES=$(wildcard $(SRCDIR)/*.c)
OBJECTS=$(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

all: build bin bin/ige

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) $< -c -o $@

.PRECIOUS: $(OBJECTS)

build:
	mkdir -p build

bin:
	mkdir -p bin

clean:
	rm -rf build bin

.PHONY: utils

bin/ige: $(OBJECTS) build bin
	$(CC) $(CFLAGS) $(LDFLAGS) $(OBJECTS) -o $@
