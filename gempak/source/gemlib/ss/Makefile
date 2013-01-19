#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)


COBJS = \
	$(ALIB)(csleep.o) \
	$(ALIB)(cssleep.o)

FOBJS = \
	$(ALIB)(sscurs.o) \
	$(ALIB)(ssenvr.o) \
	$(ALIB)(ssexit.o) \
	$(ALIB)(ssgsym.o) \
	$(ALIB)(ssnwait.o) \
	$(ALIB)(sspage.o) \
	$(ALIB)(ssvers.o) \
	$(ALIB)(sswait.o) 

GDIR = ss

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

