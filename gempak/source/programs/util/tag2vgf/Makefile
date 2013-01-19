#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = tag2vgf
PROG.c = tag2vgf.c
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
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

