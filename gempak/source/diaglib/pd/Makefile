#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(pdcape.o) \
	$(ALIB)(pdsuml.o)

COBJS = \
	$(ALIB)(pdctot.o) \
	$(ALIB)(pddden.o) \
	$(ALIB)(pddrct.o) \
	$(ALIB)(pddwpt.o) \
	$(ALIB)(pdfosb.o) \
	$(ALIB)(pdhans.o) \
	$(ALIB)(pdheat.o) \
	$(ALIB)(pdinmm.o) \
	$(ALIB)(pdkinx.o) \
	$(ALIB)(pdknms.o) \
	$(ALIB)(pdmixr.o) \
	$(ALIB)(pdmmin.o) \
	$(ALIB)(pdmskn.o) \
	$(ALIB)(pdplcl.o) \
	$(ALIB)(pdprcp.o) \
	$(ALIB)(pdprcr.o) \
	$(ALIB)(pdrelh.o) \
	$(ALIB)(pdreli.o) \
	$(ALIB)(pdrhdp.o) \
	$(ALIB)(pdsduv.o) \
	$(ALIB)(pdshmr.o) \
	$(ALIB)(pdslvp.o) \
	$(ALIB)(pdsped.o) \
	$(ALIB)(pdswet.o) \
	$(ALIB)(pdthta.o) \
	$(ALIB)(pdthte.o) \
	$(ALIB)(pdthwc.o) \
	$(ALIB)(pdtlcl.o) \
	$(ALIB)(pdtmcf.o) \
	$(ALIB)(pdtmck.o) \
	$(ALIB)(pdtmfc.o) \
	$(ALIB)(pdtmfk.o) \
	$(ALIB)(pdtmkc.o) \
	$(ALIB)(pdtmkf.o) \
	$(ALIB)(pdtmpk.o) \
	$(ALIB)(pdtmst.o) \
	$(ALIB)(pdtmwb.o) \
	$(ALIB)(pdtotl.o) \
	$(ALIB)(pdtvrk.o) \
	$(ALIB)(pduvsd.o) \
	$(ALIB)(pdvpmr.o) \
	$(ALIB)(pdvtot.o)
 
GDIR = pd

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

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

