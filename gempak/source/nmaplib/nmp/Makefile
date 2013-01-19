#
#	Makefile for nmaplib
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nmaplib.a

FOBJS = \
	$(ALIB)(nmpdspl.o)

COBJS = \
	$(ALIB)(nmpgltln.o) \
	$(ALIB)(nmpgmapattr.o) \
	$(ALIB)(nmpgmapnms.o) \
	$(ALIB)(nmpgmapnum.o) \
	$(ALIB)(nmpgovlattr.o) \
	$(ALIB)(nmpgovlflg.o) \
	$(ALIB)(nmpgovlnms.o) \
	$(ALIB)(nmpgovlnum.o) \
	$(ALIB)(nmpgtruattr.o) \
	$(ALIB)(nmpinit.o) \
	$(ALIB)(nmpmkscl.o) \
	$(ALIB)(nmpmkstn.o) \
	$(ALIB)(nmpplot.o) \
	$(ALIB)(nmprdeflts.o) \
	$(ALIB)(nmprestore.o) \
	$(ALIB)(nmprstrproj.o) \
	$(ALIB)(nmpsave.o) \
	$(ALIB)(nmpsdefmap.o) \
	$(ALIB)(nmpsetmap.o) \
	$(ALIB)(nmpsetmapstr.o) \
	$(ALIB)(nmpsimf.o) \
	$(ALIB)(nmpsimmap.o) \
	$(ALIB)(nmpsmapattr.o) \
	$(ALIB)(nmpsovlattr.o) \
	$(ALIB)(nmpsovlflg.o) \
	$(ALIB)(nmpsproj.o) \
	$(ALIB)(nmpvalid.o) \
	$(ALIB)(nmpszoom.o)

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) 

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


install: 

tlink : testnmp.c
	$(COMPILE.c) testnmp.c
	$(LINK.f) testnmp.o -o testnmp $(ALIB) \
	$(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) -lm
	$(INSTALL) testnmp $(OS_BIN)
	$(RM) *.o testnmp

clean:
	$(RM) *.o 
