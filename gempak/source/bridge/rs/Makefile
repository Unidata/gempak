#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(rsdeco.o) \
	$(ALIB)(rsdhdr.o) \
	$(ALIB)(rsdln2.o) \
	$(ALIB)(rsgchr.o) \
	$(ALIB)(rsggrp.o) \
	$(ALIB)(rsgrpt.o) \
	$(ALIB)(rshhde.o) \
	$(ALIB)(rsnmer.o) \
	$(ALIB)(rsnumd.o) \
	$(ALIB)(rsreal.o) \
	$(ALIB)(rsrrrd.o) \
	$(ALIB)(rsrtim.o) \
	$(ALIB)(rsvvde.o) \
	$(ALIB)(rswsdd.o) \
	$(ALIB)(sfstnf.o)
 
GDIR = rs

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

