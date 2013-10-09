#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = map_ssfgsf.o
 
ALIB = 

PROGS = ssfgsf
 
LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROGS) 

programs_gf:

programs_nc:

ssfgsf: $(LOBJS)
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $? $(GEMLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) $@ $(OS_BIN)
clean:
	$(RM) *.o $(CPROGS) $(PROGS)

install: all

