#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(dbgetquerytext.o) \
	$(ALIB)(dbgfilenames.o) \
	$(ALIB)(dbrunquery.o) \
	$(ALIB)(dbgetduri.o) \
	$(ALIB)(dbgetgarea.o) \
	$(ALIB)(dbgetgnav.o) \
	$(ALIB)(dbgetrdata.o) \
	$(ALIB)(dbgetstinfo.o) \
	$(ALIB)(dbgtim.o) \
	$(ALIB)(dbrdtr.o)

FOBJS = \
	$(ALIB)(dbgetcycle.o) \
	$(ALIB)(dbgetgarea.o) \
	$(ALIB)(dbgetparm.o) \
	$(ALIB)(dbinfo.o)


GDIR = db

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS) $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS_DB) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(CGEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
