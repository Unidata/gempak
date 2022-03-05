#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = gif
 
ALIB = $(OS_LIB)/gif.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hqclrs.o) \
	$(ALIB)(hsatim.o) \
	$(ALIB)(hsbrgb.o) \
	$(ALIB)(hscint.o) \
	$(ALIB)(hscolb.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hscrgb.o) \
	$(ALIB)(hsctbl.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hsicmn.o)

COBJS = \
	$(ALIB)(gd.o) \
	$(ALIB)(wbcolr.o) \
	$(ALIB)(wclear.o) \
	$(ALIB)(wclosp.o) \
	$(ALIB)(wfill.o) \
	$(ALIB)(winit.o) \
	$(ALIB)(wline.o) \
	$(ALIB)(wsatim.o) \
	$(ALIB)(wsbrgb.o) \
	$(ALIB)(wscolb.o) \
	$(ALIB)(wscolr.o) \
	$(ALIB)(wsfill.o) \
	$(ALIB)(wsicmn.o)

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

CFLAGS = $(COPT) $(XWINCDIR)

.PRECIOUS: $(ALIB)

all :  $(ALIB) $(PROG)

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


clean:
	$(RM) *.o $(PROG)

