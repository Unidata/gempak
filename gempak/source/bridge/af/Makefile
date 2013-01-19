#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(afaflv.o) \
	$(ALIB)(afalat.o) \
	$(ALIB)(afalon.o) \
	$(ALIB)(afarfg.o) \
	$(ALIB)(afatbx.o) \
	$(ALIB)(afatim.o) \
	$(ALIB)(afatmp.o) \
	$(ALIB)(afator.o) \
	$(ALIB)(afatrb.o) \
	$(ALIB)(afawnd.o) \
	$(ALIB)(afawpt.o) \
	$(ALIB)(afbkgp.o) \
	$(ALIB)(afbltp.o) \
	$(ALIB)(afcpas.o) \
	$(ALIB)(afdadr.o) \
	$(ALIB)(afdarp.o) \
	$(ALIB)(afdcod.o) \
	$(ALIB)(afdprp.o) \
	$(ALIB)(afdrco.o) \
	$(ALIB)(afdupe.o) \
	$(ALIB)(affarp.o) \
	$(ALIB)(afflvl.o) \
	$(ALIB)(afgfld.o) \
	$(ALIB)(afgmpk.o) \
	$(ALIB)(afgrpt.o) \
	$(ALIB)(afhhfm.o) \
	$(ALIB)(afhhmm.o) \
	$(ALIB)(aficid.o) \
	$(ALIB)(afifiv.o) \
	$(ALIB)(afifpt.o) \
	$(ALIB)(afifsp.o) \
	$(ALIB)(afintf.o) \
	$(ALIB)(afobdh.o) \
	$(ALIB)(afpatp.o) \
	$(ALIB)(afpflv.o) \
	$(ALIB)(afphaz.o) \
	$(ALIB)(afpicg.o) \
	$(ALIB)(afpilr.o) \
	$(ALIB)(afploc.o) \
	$(ALIB)(afplyr.o) \
	$(ALIB)(afpnbd.o) \
	$(ALIB)(afprmk.o) \
	$(ALIB)(afprsi.o) \
	$(ALIB)(afpsky.o) \
	$(ALIB)(afpsl1.o) \
	$(ALIB)(afpslr.o) \
	$(ALIB)(afptim.o) \
	$(ALIB)(afptlr.o) \
	$(ALIB)(afptmp.o) \
	$(ALIB)(afptop.o) \
	$(ALIB)(afptor.o) \
	$(ALIB)(afptrb.o) \
	$(ALIB)(afpwnd.o) \
	$(ALIB)(afpwxv.o) \
	$(ALIB)(afrcld.o) \
	$(ALIB)(africg.o) \
	$(ALIB)(afrpht.o) \
	$(ALIB)(afrswd.o) \
	$(ALIB)(afrtim.o) \
	$(ALIB)(afslat.o) \
	$(ALIB)(afslon.o) \
	$(ALIB)(aftbid.o) \
	$(ALIB)(aftmpc.o) \
	$(ALIB)(afwind.o) \
	$(ALIB)(afxbif.o)
 
GDIR = af

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

