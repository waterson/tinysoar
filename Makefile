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

rete-test: rete-test.o pool.o rete.o wmem.o

clean:
	rm -rf .deps rete-test *.o *~

