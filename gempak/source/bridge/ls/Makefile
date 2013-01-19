#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(lsaaxx.o) \
	$(ALIB)(lsast0.o) \
	$(ALIB)(lsbtyp.o) \
	$(ALIB)(lsckrp.o) \
	$(ALIB)(lscld1.o) \
	$(ALIB)(lscld4.o) \
	$(ALIB)(lsclds.o) \
	$(ALIB)(lscmmt.o) \
	$(ALIB)(lsctmp.o) \
	$(ALIB)(lsdcd1.o) \
	$(ALIB)(lsdcod.o) \
	$(ALIB)(lserrs.o) \
	$(ALIB)(lsfsnw.o) \
	$(ALIB)(lsgemp.o) \
	$(ALIB)(lsgeop.o) \
	$(ALIB)(lsgrpt.o) \
	$(ALIB)(lsicea.o) \
	$(ALIB)(lsifpt.o) \
	$(ALIB)(lsifsp.o) \
	$(ALIB)(lsiihv.o) \
	$(ALIB)(lsinit.o) \
	$(ALIB)(lsintf.o) \
	$(ALIB)(lslscb.o) \
	$(ALIB)(lsmawx.o) \
	$(ALIB)(lsnddf.o) \
	$(ALIB)(lsobst.o) \
	$(ALIB)(lspr24.o) \
	$(ALIB)(lsprec.o) \
	$(ALIB)(lspres.o) \
	$(ALIB)(lsprs3.o) \
	$(ALIB)(lsqctl.o) \
	$(ALIB)(lsrelh.o) \
	$(ALIB)(lssec1.o) \
	$(ALIB)(lssec2.o) \
	$(ALIB)(lssec3.o) \
	$(ALIB)(lssec4.o) \
	$(ALIB)(lssec5.o) \
	$(ALIB)(lssgrd.o) \
	$(ALIB)(lsswld.o) \
	$(ALIB)(lstdur.o) \
	$(ALIB)(lstemp.o) \
	$(ALIB)(lstsnw.o) \
	$(ALIB)(lswgem.o) \
	$(ALIB)(lswspd.o) \
	$(ALIB)(lswvh1.o) \
	$(ALIB)(lswvph.o)
 
GDIR = ls

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

