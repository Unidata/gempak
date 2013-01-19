#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(sfacny.o) \
	$(ALIB)(sfastn.o) \
	$(ALIB)(sfatim.o) \
	$(ALIB)(sfbegs.o) \
	$(ALIB)(sfcacny.o) \
	$(ALIB)(sfcclf.o) \
	$(ALIB)(sfcclp.o) \
	$(ALIB)(sfcgtim.o) \
	$(ALIB)(sfchkf.o) \
	$(ALIB)(sfclos.o) \
	$(ALIB)(sfcopnf.o) \
	$(ALIB)(sfcrcn.o) \
	$(ALIB)(sfcref.o) \
	$(ALIB)(sfcrfp.o) \
	$(ALIB)(sfcsdf.o) \
	$(ALIB)(sfcsdp.o) \
	$(ALIB)(sfcstn.o) \
	$(ALIB)(sfctim.o) \
	$(ALIB)(sfddat.o) \
	$(ALIB)(sfdstn.o) \
	$(ALIB)(sfdtim.o) \
	$(ALIB)(sffstn.o) \
	$(ALIB)(sfftim.o) \
	$(ALIB)(sfftyp.o) \
	$(ALIB)(sfgtim.o) \
	$(ALIB)(sfofil.o) \
	$(ALIB)(sfopnf.o) \
	$(ALIB)(sfopnr.o) \
	$(ALIB)(sfqdat.o) \
	$(ALIB)(sfqspc.o) \
	$(ALIB)(sfqstn.o) \
	$(ALIB)(sfqtxt.o) \
	$(ALIB)(sfrbox.o) \
	$(ALIB)(sfrdat.o) \
	$(ALIB)(sfrspc.o) \
	$(ALIB)(sfrstr.o) \
	$(ALIB)(sfsnxt.o) \
	$(ALIB)(sfsstn.o) \
	$(ALIB)(sfstat.o) \
	$(ALIB)(sfstid.o) \
	$(ALIB)(sfstim.o) \
	$(ALIB)(sfstst.o) \
	$(ALIB)(sftlst.o) \
	$(ALIB)(sftnxt.o) \
	$(ALIB)(sftstn.o) \
	$(ALIB)(sfttim.o) \
	$(ALIB)(sfuare.o) \
	$(ALIB)(sfustn.o) \
	$(ALIB)(sfwbox.o) \
	$(ALIB)(sfwdat.o) \
	$(ALIB)(sfwsdd.o) \
	$(ALIB)(sfwspc.o) \
	$(ALIB)(sfwstr.o)
 
GDIR = sf

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

