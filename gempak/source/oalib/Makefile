#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(oabnd.o)

FOBJS = \
	$(ALIB)(oabarn.o) \
	$(ALIB)(oaboxc.o) \
	$(ALIB)(oagfil.o) \
	$(ALIB)(oagint.o) \
	$(ALIB)(oagues.o) \
	$(ALIB)(oaigrd.o) \
	$(ALIB)(oaltln.o) \
	$(ALIB)(oanavg.o) \
	$(ALIB)(oasint.o) \
	$(ALIB)(oawfsr.o) \
	$(ALIB)(oawgrd.o) \
	$(ALIB)(oawrms.o)
 
GDIR = oa

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

