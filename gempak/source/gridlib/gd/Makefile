#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(gdaddt.o) \
	$(ALIB)(gdcpf.o) \
	$(ALIB)(gdclos.o) \
	$(ALIB)(gdcref.o) \
	$(ALIB)(gddelt.o) \
	$(ALIB)(gddgrd.o) \
	$(ALIB)(gdfchk.o) \
	$(ALIB)(gdfltm.o) \
	$(ALIB)(gdganl.o) \
	$(ALIB)(gdgeni.o) \
	$(ALIB)(gdgcyc.o) \
	$(ALIB)(gdggrd.o) \
	$(ALIB)(gdgidn.o) \
	$(ALIB)(gdginf.o) \
	$(ALIB)(gdglev.o) \
	$(ALIB)(gdglevw.o) \
	$(ALIB)(gdgnav.o) \
	$(ALIB)(gdgnum.o) \
	$(ALIB)(gdgnumw.o) \
	$(ALIB)(gdgtim.o) \
	$(ALIB)(gdgtimw.o) \
	$(ALIB)(gdgtmf.o) \
	$(ALIB)(gdhtoi.o) \
	$(ALIB)(gdinit.o) \
	$(ALIB)(gditoh.o) \
	$(ALIB)(gdngrd.o) \
	$(ALIB)(gdofil.o) \
	$(ALIB)(gdopen.o) \
	$(ALIB)(gdopnf.o) \
	$(ALIB)(gdopnr.o) \
	$(ALIB)(gdrdat.o) \
	$(ALIB)(gdrdatw.o) \
	$(ALIB)(gdsrtl.o) \
	$(ALIB)(gdsrtt.o) \
	$(ALIB)(gdswrt.o) \
	$(ALIB)(gdwdat.o) \
	$(ALIB)(gdwpgd.o) \
	$(ALIB)(gdwpgdw.o) \
	$(ALIB)(gdwpg2.o) \
	$(ALIB)(gdwppg.o) \
	$(ALIB)(gdwppgw.o) 
 
GDIR = gd 

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(CGEMLIB) $(GEMLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

