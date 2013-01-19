#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = 
 
ALIB = 
 
PROG = datetime
PROG.c = datetime.c

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB)

all : $(PROG) 

$(PROG): $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(ALIB) $(LIBINC) -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG) $(PROG1)
	$(INSTALL) $(PROG) $(OS_BIN)

