include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS =

ALIB =

PROG = joinvgf

all: $(PROG)

joinvgf:
	$(COMPILE.c) joinvgf.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $(GPLT_NC) \
		$(CGEMLIB) $(GEMLIB) $(GPLT) \
		$(DEVICE) $(GN) $(GEMLIB) $(CGEMLIB) $(GEMLIB) -lm
	$(RM) *.o

install: all
	$(INSTALL) joinvgf $(OS_BIN)

clean:
	$(RM) $(PROG) *.o
