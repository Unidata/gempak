#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(mn3chc.o) \
	$(ALIB)(mn3chr.o) \
	$(ALIB)(mndecd.o) \
	$(ALIB)(mnfctm.o) \
	$(ALIB)(mnglin.o) \
	$(ALIB)(mngrpt.o) \
	$(ALIB)(mnntdy.o) \
	$(ALIB)(mnsls.o) \
	$(ALIB)(mntitl.o)
 
GDIR = mn

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

