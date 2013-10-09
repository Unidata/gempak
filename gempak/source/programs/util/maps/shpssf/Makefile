#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = 
 
ALIB = 

CPROGS = shpssf
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(CPROGS)

programs_gf:

programs_nc:

$(CPROGS):
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) $@.o $(GPLT_NC) $(CGEMLIB) $(GPLT) \
		$(DEVICE) $(NC) $(GN) $(GEMLIB) \
		$(CGEMLIB) $(GEMLIB) -lm -o $@
	$(INSTALL) $@ $(OS_BIN)
	$(RM) *.o

clean:
	$(RM) *.o $(CPROGS) $(PROGS)

install: all

