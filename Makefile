# Generated automatically from Makefile.in by configure.
ifdef BUILD_OPT
# Optimized
CFLAGS=-Wall -O -UDEBUG -DNDEBUG
else
# Debug
CFLAGS=-Wall -g -DDEBUG -UNDEBUG
endif

MDDEPDIR=.deps

.c.o:
	$(CC) -c $(CFLAGS) -Wp,-MD,$(MDDEPDIR)/$*.pp -o $@ $<

all: $(MDDEPDIR) rete-test

MDDEPFILES := $(wildcard $(MDDEPDIR)/*.pp)

$(MDDEPDIR):
	mkdir $@

ifdef MDDEPFILES
include $(MDDEPFILES)
endif

OBJS = \
	alloc.o \
	agent.o \
	ht.o \
	pref.o \
	rete-test.o \
	rete.o \
	runtime.o \
	symtab.o \
	tmem.o \
	wmem.o \
	$(NULL)

rete-test: $(OBJS)

clean:
	rm -rf .deps rete-test *.o *~

distclean: clean
	rm -f config.cache config.log config.status config-defs.h

