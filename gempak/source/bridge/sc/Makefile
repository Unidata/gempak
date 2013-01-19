#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(sccltp.o) \
	$(ALIB)(scdcod.o) \
	$(ALIB)(scdecd.o) \
	$(ALIB)(scgemp.o) \
	$(ALIB)(scgrpt.o) \
	$(ALIB)(scifpt.o) \
	$(ALIB)(scifsp.o) \
	$(ALIB)(scinit.o) \
	$(ALIB)(scintf.o) \
	$(ALIB)(scprrm.o) \
	$(ALIB)(scrptm.o) \
	$(ALIB)(sctprm.o) \
	$(ALIB)(scwea3.o)
 
GDIR = sc

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

