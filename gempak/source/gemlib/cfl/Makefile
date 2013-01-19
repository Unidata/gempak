#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(cflaopn.o) \
	$(ALIB)(cflclos.o) \
	$(ALIB)(cfldopn.o) \
	$(ALIB)(cflgfil.o) \
	$(ALIB)(cflinqr.o) \
	$(ALIB)(cfliret.o) \
	$(ALIB)(cflisdir.o) \
	$(ALIB)(cflmdat.o) \
	$(ALIB)(cflmnam.o) \
	$(ALIB)(cflpath.o) \
	$(ALIB)(cflperms.o) \
	$(ALIB)(cflrdir.o) \
	$(ALIB)(cflrdln.o) \
	$(ALIB)(cflread.o) \
	$(ALIB)(cflropn.o) \
	$(ALIB)(cflscandir.o) \
	$(ALIB)(cflscnd.o) \
	$(ALIB)(cflscnt.o) \
	$(ALIB)(cflseek.o) \
	$(ALIB)(cflsrch.o) \
	$(ALIB)(cfltbnr.o) \
	$(ALIB)(cfltbop.o) \
	$(ALIB)(cfltinq.o) \
	$(ALIB)(cfltmpo.o) \
	$(ALIB)(cfltrln.o) \
	$(ALIB)(cfluopn.o) \
	$(ALIB)(cflwher.o) \
	$(ALIB)(cflwopn.o) \
	$(ALIB)(cflwrit.o)
 
GDIR = cfl

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

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

