#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(dvadv.o) \
	$(ALIB)(dvage.o) \
	$(ALIB)(dvavor.o) \
	$(ALIB)(dvcirc.o) \
	$(ALIB)(dvcros.o) \
	$(ALIB)(dvdef.o) \
	$(ALIB)(dvdirn.o) \
	$(ALIB)(dvdirr.o) \
	$(ALIB)(dvdiv.o) \
	$(ALIB)(dvdivt.o) \
	$(ALIB)(dvdot.o) \
	$(ALIB)(dvdsub.o) \
	$(ALIB)(dvdvdx.o) \
	$(ALIB)(dvdvdy.o) \
	$(ALIB)(dvfrnt.o) \
	$(ALIB)(dvgcir.o) \
	$(ALIB)(dvgcwv.o) \
	$(ALIB)(dvgeo.o) \
	$(ALIB)(dvgrad.o) \
	$(ALIB)(dvgwfv.o) \
	$(ALIB)(dvinad.o) \
	$(ALIB)(dvisal.o) \
	$(ALIB)(dvkcrs.o) \
	$(ALIB)(dvkntv.o) \
	$(ALIB)(dvltrn.o) \
	$(ALIB)(dvmag.o) \
	$(ALIB)(dvmdiv.o) \
	$(ALIB)(dvmrad.o) \
	$(ALIB)(dvmsdv.o) \
	$(ALIB)(dvmsfc.o) \
	$(ALIB)(dvmtng.o) \
	$(ALIB)(dvnorm.o) \
	$(ALIB)(dvnrmv.o) \
	$(ALIB)(dvobs.o) \
	$(ALIB)(dvpvor.o) \
	$(ALIB)(dvpvr3.o) \
	$(ALIB)(dvqvcl.o) \
	$(ALIB)(dvqvec.o) \
	$(ALIB)(dvrad.o) \
	$(ALIB)(dvrdfv.o) \
	$(ALIB)(dvrich.o) \
	$(ALIB)(dvross.o) \
	$(ALIB)(dvrot.o) \
	$(ALIB)(dvsdiv.o) \
	$(ALIB)(dvshr.o) \
	$(ALIB)(dvsm5v.o) \
	$(ALIB)(dvsmul.o) \
	$(ALIB)(dvsquo.o) \
	$(ALIB)(dvstr.o) \
	$(ALIB)(dvtang.o) \
	$(ALIB)(dvthrm.o) \
	$(ALIB)(dvtng.o) \
	$(ALIB)(dvtngv.o) \
	$(ALIB)(dvun.o) \
	$(ALIB)(dvur.o) \
	$(ALIB)(dvvadd.o) \
	$(ALIB)(dvvasv.o) \
	$(ALIB)(dvvavs.o) \
	$(ALIB)(dvvbtw.o) \
	$(ALIB)(dvvecn.o) \
	$(ALIB)(dvvecr.o) \
	$(ALIB)(dvvesd.o) \
	$(ALIB)(dvvge.o) \
	$(ALIB)(dvvgt.o) \
	$(ALIB)(dvvlav.o) \
	$(ALIB)(dvvldf.o) \
	$(ALIB)(dvvle.o) \
	$(ALIB)(dvvlt.o) \
	$(ALIB)(dvvmsk.o) \
	$(ALIB)(dvvmul.o) \
	$(ALIB)(dvvn.o) \
	$(ALIB)(dvvor.o) \
	$(ALIB)(dvvquo.o) \
	$(ALIB)(dvvr.o) \
	$(ALIB)(dvvsub.o) \
	$(ALIB)(dvwnan.o) \
	$(ALIB)(dvwshr.o)
 
GDIR = dv

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
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

