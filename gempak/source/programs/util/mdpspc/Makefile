#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG_SPC = mdpspc
PROG_SPC.c = mdpspc.c

LIBINC = \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROG_SPC)

$(PROG_SPC): $(LIBINC) $(PROG_SPC.c)
	$(COMPILE.c) $(PROG_SPC.c) 
	$(LINK.f) $(NOFORMAIN) $@.o $(GPLT_NC) \
        $(CGEMLIB) $(GPLT) $(DEVICE) $(OS_LIB)/gif.a $(OS_LIB)/gn.a \
        $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(GPLT) $(DEVICE) -lm -o $@

clean:
	$(RM) *.o $(PROG_SPC)

install: $(PROG_SPC)
	$(INSTALL) $(PROG_SPC) $(OS_BIN)

