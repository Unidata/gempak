#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = 
 
ALIB = 
 
PROG = vgftoascii
PROG.c = vgftoascii.c

LIBINC = \
	$(CGEMLIB) \
	$(APPL) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(GEMLIB) \
	$(SYSLIB)

all : $(PROG)

CFLAGS = $(COPT) -I$(NAWIPS)/include

$(PROG): $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(ALIB) $(LIBINC) -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
