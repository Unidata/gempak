#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = dcwmostrip
PROG.c = dcwmostrip.c
 

LIBINC = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c) 
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(SYSLIBS) -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

