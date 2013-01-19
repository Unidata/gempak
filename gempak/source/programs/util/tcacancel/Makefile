include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS =

ALIB =

PROG = tcacancel

all: $(PROG)

$(PROG): $(PROG).c
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $(GPLT_NC) \
		$(GEMLIB) $(OS_LIB)/textlib.a $(CGEMLIB) $(GPLT) \
		$(DEVICE) $(CGEMLIB) $(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) $(GN) -lm
	$(RM) *.o

install: all
	$(INSTALL) $(PROG) $(OS_BIN)

clean:
	$(RM) $(PROG) *.o
