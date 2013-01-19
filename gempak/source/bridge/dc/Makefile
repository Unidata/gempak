#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

COBJS = \
	$(ALIB)(dcb.o) \
	$(ALIB)(dcdira.o) \
	$(ALIB)(dcdlog.o) \
	$(ALIB)(dcexit.o) \
	$(ALIB)(dcgbul.o) \
	$(ALIB)(dcgopt.o) \
	$(ALIB)(dcinit.o) \
	$(ALIB)(dcsgnl.o) \
	$(ALIB)(dcwbuf.o) \
	$(ALIB)(dcwclg.o)

FOBJS = \
	$(ALIB)(dcbsrc.o) \
	$(ALIB)(dcbsrh.o) \
	$(ALIB)(dccrid.o) \
	$(ALIB)(dcfcls.o) \
	$(ALIB)(dcfcyl.o) \
	$(ALIB)(dcfint.o) \
	$(ALIB)(dcghdr.o) \
	$(ALIB)(dcgpil.o) \
	$(ALIB)(dcgtim.o) \
	$(ALIB)(dciffp.o) \
	$(ALIB)(dcifmp.o) \
	$(ALIB)(dcifpc.o) \
	$(ALIB)(dcifpr.o) \
	$(ALIB)(dcitim.o) \
	$(ALIB)(dcosfc.o) \
	$(ALIB)(dcosfr.o) \
	$(ALIB)(dcoshp.o) \
	$(ALIB)(dcosnd.o) \
	$(ALIB)(dcosnm.o) \
	$(ALIB)(dcrtim.o) \
	$(ALIB)(dcrtmh.o) \
	$(ALIB)(dcstns.o) \
	$(ALIB)(dctest.o) \
	$(ALIB)(dctmck.o) \
	$(ALIB)(dctmst.o) \
	$(ALIB)(dcwlog.o) \
	$(ALIB)(dcwthr.o)
 
GDIR = dc

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

