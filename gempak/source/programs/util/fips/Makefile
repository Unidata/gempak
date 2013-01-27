#
#	Makefile for GEMPAK 6.0
#		Builds programs
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = 

PROG.c = fips.c

LOBJS = 

COBJS = 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

PROG = $(PROG.c:.c=)
PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

CFLAGS = $(COPT)

all : $(SYMLINKS) $(COBJS) $(PROG)

$(PROG) : $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $(PROG).o $(COBJS) $(LIBINC) -o $@

programs_nc : 

programs_gf : 



clean:
	$(RM) $(PROG) *.o $(ALIB)


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
