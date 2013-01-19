#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(lcabnd.o) \
	$(ALIB)(lcarea.o) \
	$(ALIB)(lccoun.o) \
	$(ALIB)(lcdist.o) \
	$(ALIB)(lcfloc.o) \
	$(ALIB)(lcfstn.o) \
	$(ALIB)(lcgare.o) \
	$(ALIB)(lcinbn.o) \
	$(ALIB)(lcsare.o) \
	$(ALIB)(lcsbnd.o) \
	$(ALIB)(lcuare.o)
 
GDIR = lc

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

