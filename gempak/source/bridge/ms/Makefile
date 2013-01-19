#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(mscdat.o) \
	$(ALIB)(msclim.o) \
	$(ALIB)(msdate.o) \
	$(ALIB)(msdcda.o) \
	$(ALIB)(msdcdm.o) \
	$(ALIB)(msfctm.o) \
	$(ALIB)(msglin.o) \
	$(ALIB)(msgrpt.o) \
	$(ALIB)(msidat.o) \
	$(ALIB)(msidt2.o) \
	$(ALIB)(msmiss.o) \
	$(ALIB)(msmxmn.o) \
	$(ALIB)(msopnc.o) \
	$(ALIB)(msptyp.o) \
	$(ALIB)(msstid.o)
 
GDIR = ms

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

