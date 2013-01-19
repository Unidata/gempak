#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/textlib.a

COBJS = \
	$(ALIB)(wbcarea.o) \
	$(ALIB)(wbcdavn.o) \
	$(ALIB)(wbcdcon.o) \
	$(ALIB)(wbcdcty.o) \
	$(ALIB)(wbcdelf.o) \
	$(ALIB)(wbcdhdl.o) \
	$(ALIB)(wbcdhwm.o) \
	$(ALIB)(wbcdsts.o) \
	$(ALIB)(wbcmzhv.o) \
	$(ALIB)(wbcmzrm.o) \
	$(ALIB)(wbcvors.o) \
	$(ALIB)(wbcwcp.o)

GDIR = wbc

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

