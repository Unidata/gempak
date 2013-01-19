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
 
CPROGS = shpcv shpssf
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROGS) $(CPROGS)

programs_gf:

programs_nc:

ssfgsf: $(LOBJS)
	$(COMPILE.c) $@.c
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $? $(GEMLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) $@ $(OS_BIN)


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

