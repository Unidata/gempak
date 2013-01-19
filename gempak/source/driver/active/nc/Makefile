#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = nc
 
ALIB = $(OS_LIB)/nc.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hmesg.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hqcomp.o) \
	$(ALIB)(hqdatt.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hslwid.o) \
	$(ALIB)(hstext.o) \
	$(ALIB)(htext.o) \
	$(ALIB)(htextc.o) \
	$(ALIB)(mclear.o) \
	$(ALIB)(mclose.o) \
	$(ALIB)(mdots.o) \
	$(ALIB)(mfill.o) \
	$(ALIB)(minita.o) \
	$(ALIB)(mline.o) \
	$(ALIB)(mmesg.o) \
	$(ALIB)(mopen.o) \
	$(ALIB)(mqcomp.o) \
	$(ALIB)(mqdatt.o) \
	$(ALIB)(mrflhd.o) \
	$(ALIB)(mrfrhd.o) \
	$(ALIB)(mscolr.o) \
	$(ALIB)(msdatt.o) \
	$(ALIB)(msfill.o) \
	$(ALIB)(mslwid.o) \
	$(ALIB)(mstext.o) \
	$(ALIB)(mtext.o) \
	$(ALIB)(mtextc.o) \
	$(ALIB)(mwflhd.o) \
	$(ALIB)(mwfrhd.o)

LIBINC = \
	$(GEMLIB) \
	$(DEVICE) \
	$(ALIB) \
	$(CGEMLIB) \
	$(DEVLIBS) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(SYSLIB) \
	$(NEEDX11) \
	-lm

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : device.f $(ALIB) $(PROG)

install:
	$(INSTALL) $(PROG) $(OS_BIN)

$(PROG): $(ALIB)
	$(LINK.f) device.f $(LIBINC) -o $@
	$(RM) device.o

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


device.f :
	$(LN) $(GEMPAK)/source/device/main/device.f .

clean:
	$(RM) *.o device.f $(PROG)

