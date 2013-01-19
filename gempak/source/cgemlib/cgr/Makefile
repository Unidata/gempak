#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(cgrbisectpt.o) \
	$(ALIB)(cgrbounds.o) \
	$(ALIB)(cgrcanbeformatted.o) \
	$(ALIB)(cgrcentroid.o) \
	$(ALIB)(cgrcntstrks.o) \
	$(ALIB)(cgrconcave.o) \
	$(ALIB)(cgrcsegint.o) \
	$(ALIB)(cgrdang.o) \
	$(ALIB)(cgrdist.o) \
	$(ALIB)(cgrdone.o) \
	$(ALIB)(cgrgpc.o) \
	$(ALIB)(cgrinit.o) \
	$(ALIB)(cgrinpoly.o) \
	$(ALIB)(cgrinpolywn.o) \
	$(ALIB)(cgrinsert.o) \
	$(ALIB)(cgrintersect.o) \
	$(ALIB)(cgrlindist.o) \
	$(ALIB)(cgrlineinterp.o) \
	$(ALIB)(cgrlinelen.o) \
	$(ALIB)(cgrlinepoly.o) \
	$(ALIB)(cgrlinepolyint.o) \
	$(ALIB)(cgrntrsct.o) \
	$(ALIB)(cgrobjinpoly.o) \
	$(ALIB)(cgrobjint.o) \
	$(ALIB)(cgrordrccw.o) \
	$(ALIB)(cgrpoly.o) \
	$(ALIB)(cgrpolydiff.o) \
	$(ALIB)(cgrpolyint.o) \
	$(ALIB)(cgrpolyinterp.o) \
	$(ALIB)(cgrpolylink.o) \
	$(ALIB)(cgrpolyp.o) \
	$(ALIB)(cgrpolysmear.o) \
	$(ALIB)(cgrpolyunion.o) \
	$(ALIB)(cgrptonln.o) \
	$(ALIB)(cgrrange.o) \
	$(ALIB)(cgrreducelinepoly.o) \
	$(ALIB)(cgrreducepts.o) \
	$(ALIB)(cgrreduceptsw.o) \
	$(ALIB)(cgrreorder.o) \
	$(ALIB)(cgrrolseg.o) \
	$(ALIB)(cgrqrol.o) \
	$(ALIB)(cgrqsol.o) \
	$(ALIB)(cgrsegdist.o) \
	$(ALIB)(cgrsegint.o) \
	$(ALIB)(cgrsegintwn.o) \
	$(ALIB)(cgrsphpolyarea.o) \
	$(ALIB)(cgrtorad.o) \
	$(ALIB)(cgrvectxprod.o) \
	$(ALIB)(cgrynxt.o)
 
GDIR = cgr

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

