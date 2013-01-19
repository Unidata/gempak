#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(ptactp.o) \
	$(ALIB)(ptccnm.o) \
	$(ALIB)(ptcldn.o) \
	$(ALIB)(ptclds.o) \
	$(ALIB)(ptcmcl.o) \
	$(ALIB)(ptcnvr.o) \
	$(ALIB)(ptdigr.o) \
	$(ALIB)(ptdpdx.o) \
	$(ALIB)(ptdprc.o) \
	$(ALIB)(ptfqot.o) \
	$(ALIB)(ptfunc.o) \
	$(ALIB)(ptpatn.o) \
	$(ALIB)(ptpwth.o) \
	$(ALIB)(ptsalt.o) \
	$(ALIB)(ptswel.o) \
	$(ALIB)(pttice.o) \
	$(ALIB)(pttpfc.o) \
	$(ALIB)(ptturb.o) \
	$(ALIB)(ptvsbc.o) \
	$(ALIB)(ptvsbf.o) \
	$(ALIB)(ptwcod.o) \
	$(ALIB)(ptwnmt.o) \
	$(ALIB)(ptwnum.o) \
	$(ALIB)(ptwsym.o) \
	$(ALIB)(ptwtmo.o)
 
GDIR = pt

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

