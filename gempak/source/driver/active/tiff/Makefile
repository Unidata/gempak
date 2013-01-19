#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = tiff
 
ALIB = $(OS_LIB)/tiff.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hdots.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hsatim.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hsicmn.o) \
	$(ALIB)(tbrescirc.o) \
	$(ALIB)(tcircpts.o) \
	$(ALIB)(tclear.o) \
	$(ALIB)(tclosp.o) \
	$(ALIB)(tdots.o) \
	$(ALIB)(tendd.o) \
	$(ALIB)(tfill.o) \
	$(ALIB)(tg4comp.o) \
	$(ALIB)(tinita.o) \
	$(ALIB)(tline.o) \
	$(ALIB)(tsatim.o) \
	$(ALIB)(tscnfll.o) \
	$(ALIB)(tscolr.o) \
	$(ALIB)(tsdatt.o) \
	$(ALIB)(tsfill.o) \
	$(ALIB)(tsicmn.o) \
	$(ALIB)(tsopen.o) \
	$(ALIB)(tspan.o) \
	$(ALIB)(ttiff.o) \
	$(ALIB)(tuncomp.o) \
	$(ALIB)(twrpxl.o)

LIBINC = \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
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

