#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(snastn.o) \
	$(ALIB)(snatim.o) \
	$(ALIB)(snbegs.o) \
	$(ALIB)(snchkf.o) \
	$(ALIB)(snckua.o) \
	$(ALIB)(snclos.o) \
	$(ALIB)(sncref.o) \
	$(ALIB)(sncrfp.o) \
	$(ALIB)(sncrua.o) \
	$(ALIB)(sncstn.o) \
	$(ALIB)(snctim.o) \
	$(ALIB)(snddat.o) \
	$(ALIB)(sndstn.o) \
	$(ALIB)(sndtim.o) \
	$(ALIB)(snfstn.o) \
	$(ALIB)(snftim.o) \
	$(ALIB)(sngclm.o) \
	$(ALIB)(snghgt.o) \
	$(ALIB)(sngtim.o) \
	$(ALIB)(snintc.o) \
	$(ALIB)(snmand.o) \
	$(ALIB)(snmerg.o) \
	$(ALIB)(snmght.o) \
	$(ALIB)(snmman.o) \
	$(ALIB)(snmspm.o) \
	$(ALIB)(snmtyp.o) \
	$(ALIB)(snofil.o) \
	$(ALIB)(snopnf.o) \
	$(ALIB)(snopnr.o) \
	$(ALIB)(snopnt.o) \
	$(ALIB)(snqdat.o) \
	$(ALIB)(snqstn.o) \
	$(ALIB)(snrdat.o) \
	$(ALIB)(snrprt.o) \
	$(ALIB)(snrstr.o) \
	$(ALIB)(snrtyp.o) \
	$(ALIB)(snrtbl.o) \
	$(ALIB)(snsnxt.o) \
	$(ALIB)(snsstn.o) \
	$(ALIB)(snstid.o) \
	$(ALIB)(snstim.o) \
	$(ALIB)(sntnxt.o) \
	$(ALIB)(sntstn.o) \
	$(ALIB)(snttim.o) \
	$(ALIB)(snustn.o) \
	$(ALIB)(snwdat.o) \
	$(ALIB)(snwprt.o) \
	$(ALIB)(snwstr.o)
 
GDIR = sn

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(CGEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

