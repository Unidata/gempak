#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS = 

ALIB = 
 
PROG = vgmod

PROG.c = vgmod.c

LIBINC = \
	$(CGEMLIB) \
	$(APPL) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(GEMLIB) \
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

