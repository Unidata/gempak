#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS = 

ALIB = 
 
PROG = vgToSGWX

PROG.c = vgToSGWX.c

LIBINC = \
	$(OS_LIB)/gendp_alt.o \
	$(OS_LIB)/ginitp_alt.o \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(APPL) \
	$(GPLT) \
	$(CGEMLIB) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	$(GPLT) \
	$(SYSLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) -lm -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

