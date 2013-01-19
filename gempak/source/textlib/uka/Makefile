#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/textlib.a

COBJS = \
	$(ALIB)(ukagrpchk.o) \
	$(ALIB)(ukajtin.o) \
	$(ALIB)(ukajtsp.o) \
	$(ALIB)(ukaptcld.o) \
	$(ALIB)(ukaptfrt.o) \
	$(ALIB)(ukaptjet.o) \
	$(ALIB)(ukaptmcld.o) \
	$(ALIB)(ukaptrad.o) \
	$(ALIB)(ukaptstm.o) \
	$(ALIB)(ukapttrp.o) \
	$(ALIB)(ukapttur.o) \
	$(ALIB)(ukaptvol.o)

GDIR = uka

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

