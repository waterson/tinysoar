ifdef BUILD_OPT
# Optimized
CFLAGS=-Wall -O -UDEBUG -DNDEBUG -DINLINE=inline
else
# Debug
CFLAGS=-Wall -g -DDEBUG -UNDEBUG -DINLINE=
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
	pool.o \
	pref.o \
	rete-test.o \
	rete.o \
	runtime.o \
	symtab.o \
	wmem.o \
	$(NULL)

rete-test: $(OBJS)

clean:
	rm -rf .deps rete-test *.o *~

