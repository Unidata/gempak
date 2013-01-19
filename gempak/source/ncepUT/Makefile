#
#	Makefile for GEMPAK 6.0
#		Builds libncepUT.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libncepUT.a

LOBJS = \
	$(ALIB)(dbn_bufr.o) \
	$(ALIB)(ut85dy.o) \
	$(ALIB)(utbfci.o) \
	$(ALIB)(utbfms.o) \
	$(ALIB)(utbfpa.o) \
	$(ALIB)(utbfpm.o) \
	$(ALIB)(utbfpw.o) \
	$(ALIB)(utbfri.o) \
	$(ALIB)(utbkgp.o) \
	$(ALIB)(utbmri.o) \
	$(ALIB)(utc2r8.o) \
	$(ALIB)(utcbs3.o) \
	$(ALIB)(utcibf.o) \
	$(ALIB)(utcurr.o) \
	$(ALIB)(utemsg.o) \
	$(ALIB)(utiwbf.o) \
	$(ALIB)(utmdqi.o) \
	$(ALIB)(utmdri.o) \
	$(ALIB)(utqibf.o) \
	$(ALIB)(utr82c.o) \
	$(ALIB)(utribf.o) \
	$(ALIB)(utribm.o) \
	$(ALIB)(utsec3.o) \
	$(ALIB)(utwbfr.o)
 
.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(LOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o

programs_nc:

programs_gf:

