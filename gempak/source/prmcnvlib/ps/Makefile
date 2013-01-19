#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(psbrch.o) \
	$(ALIB)(psbvfq.o) \
	$(ALIB)(psbvpd.o) \
	$(ALIB)(psbvsq.o) \
	$(ALIB)(pscape.o) \
	$(ALIB)(pscins.o) \
	$(ALIB)(psctot.o) \
	$(ALIB)(pseqlv.o) \
	$(ALIB)(pshans.o) \
	$(ALIB)(pskinx.o) \
	$(ALIB)(pslaps.o) \
	$(ALIB)(pslcll.o) \
	$(ALIB)(pslfcv.o) \
	$(ALIB)(pslift.o) \
	$(ALIB)(psmlmr.o) \
	$(ALIB)(psmlth.o) \
	$(ALIB)(psprcl.o) \
	$(ALIB)(psrich.o) \
	$(ALIB)(pssepa.o) \
	$(ALIB)(psshow.o) \
	$(ALIB)(psshrd.o) \
	$(ALIB)(psshrm.o) \
	$(ALIB)(psstab.o) \
	$(ALIB)(psstap.o) \
	$(ALIB)(psswet.o) \
	$(ALIB)(pstotl.o) \
	$(ALIB)(psustb.o) \
	$(ALIB)(psvtot.o)
 
GDIR = ps

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

