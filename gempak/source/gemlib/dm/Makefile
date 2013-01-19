#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(dmrpkgc.o) \
	$(ALIB)(dmrpkgc2.o) \
	$(ALIB)(dmwdtrc.o)

FOBJS = \
	$(ALIB)(dmafre.o) \
	$(ALIB)(dmaspc.o) \
	$(ALIB)(dmbegs.o) \
	$(ALIB)(dmchkf.o) \
	$(ALIB)(dmchng.o) \
	$(ALIB)(dmclop.o) \
	$(ALIB)(dmclos.o) \
	$(ALIB)(dmcnst.o) \
	$(ALIB)(dmcond.o) \
	$(ALIB)(dmcret.o) \
	$(ALIB)(dmcsrc.o) \
	$(ALIB)(dmdall.o) \
	$(ALIB)(dmdclh.o) \
	$(ALIB)(dmdcsr.o) \
	$(ALIB)(dmddat.o) \
	$(ALIB)(dmdelt.o) \
	$(ALIB)(dmdpsr.o) \
	$(ALIB)(dmdrwh.o) \
	$(ALIB)(dmefre.o) \
	$(ALIB)(dmespc.o) \
	$(ALIB)(dmfkey.o) \
	$(ALIB)(dmfspc.o) \
	$(ALIB)(dmfwrt.o) \
	$(ALIB)(dmgetf.o) \
	$(ALIB)(dmgetgnav.o) \
	$(ALIB)(dmggpm.o) \
	$(ALIB)(dmgspc.o) \
	$(ALIB)(dmgtim.o) \
	$(ALIB)(dmkeys.o) \
	$(ALIB)(dmlssf.o) \
	$(ALIB)(dmlstn.o) \
	$(ALIB)(dmltim.o) \
	$(ALIB)(dmnext.o) \
	$(ALIB)(dmnxtc.o) \
	$(ALIB)(dmopen.o) \
	$(ALIB)(dmpack.o) \
	$(ALIB)(dmpart.o) \
	$(ALIB)(dmpkgd.o) \
	$(ALIB)(dmpnam.o) \
	$(ALIB)(dmpsrc.o) \
	$(ALIB)(dmqdat.o) \
	$(ALIB)(dmrch4.o) \
	$(ALIB)(dmrclh.o) \
	$(ALIB)(dmrdmg.o) \
	$(ALIB)(dmrdtc.o) \
	$(ALIB)(dmrdti.o) \
	$(ALIB)(dmrdtr.o) \
	$(ALIB)(dmrfhc.o) \
	$(ALIB)(dmrfhi.o) \
	$(ALIB)(dmrfhr.o) \
	$(ALIB)(dmrfil.o) \
	$(ALIB)(dmrflt.o) \
	$(ALIB)(dmrhda.o) \
	$(ALIB)(dmrint.o) \
	$(ALIB)(dmrkey.o) \
	$(ALIB)(dmrlbl.o) \
	$(ALIB)(dmrpkg.o) \
	$(ALIB)(dmrprt.o) \
	$(ALIB)(dmrrec.o) \
	$(ALIB)(dmrrwh.o) \
	$(ALIB)(dmrstr.o) \
	$(ALIB)(dmsgpk.o) \
	$(ALIB)(dmsrch.o) \
	$(ALIB)(dmunpk.o) \
	$(ALIB)(dmwch4.o) \
	$(ALIB)(dmwclh.o) \
	$(ALIB)(dmwdmg.o) \
	$(ALIB)(dmwdtc.o) \
	$(ALIB)(dmwdti.o) \
	$(ALIB)(dmwdtr.o) \
	$(ALIB)(dmwfhc.o) \
	$(ALIB)(dmwfhi.o) \
	$(ALIB)(dmwfhr.o) \
	$(ALIB)(dmwfil.o) \
	$(ALIB)(dmwflt.o) \
	$(ALIB)(dmwgb2.o) \
	$(ALIB)(dmwhda.o) \
	$(ALIB)(dmwint.o) \
	$(ALIB)(dmwkey.o) \
	$(ALIB)(dmwlbl.o) \
	$(ALIB)(dmword.o) \
	$(ALIB)(dmwpkg.o) \
	$(ALIB)(dmwprt.o) \
	$(ALIB)(dmwrwh.o) \
	$(ALIB)(dmwspc.o) \
	$(ALIB)(dmwstr.o)
 
GDIR = dm

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

