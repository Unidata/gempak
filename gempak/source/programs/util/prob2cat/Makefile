include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS =

ALIB =

PROG = prob2cat

all: $(PROG)

$(PROG): $(PROG).c
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o \
		$(OS_LIB)/textlib.a $(CGEMLIB) $(GEMLIB) $(CGEMLIB)  \
		$(LIBGPC) $(GPLT_NC) \
		$(GPLT) $(DEVICE) $(GN) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(XMLLIB) \
		$(OS_LIB)/libxml2.a $(XMLDEP) $(ZLIB) -lm
	$(RM) *.o

install: all
	$(INSTALL) $(PROG) $(OS_BIN)

clean:
	$(RM) $(PROG) *.o
