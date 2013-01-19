#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(ergerrmsg.o) \
	$(ALIB)(ergnumerr.o) \
	$(ALIB)(erinit.o) \
	$(ALIB)(ersmsg.o) \
	$(ALIB)(erwbuf.o)

FOBJS = \
	$(ALIB)(erlmsg.o) \
	$(ALIB)(ermmsg.o) \
	$(ALIB)(erstat.o) \
	$(ALIB)(erwmsg.o)
 
GDIR = er

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

