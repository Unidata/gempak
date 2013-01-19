#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = png
 
ALIB = $(OS_LIB)/png.a

DEVLIBS = $(GN)
 
COBJS =

LOBJS = \
	$(ALIB)(xclosp.o) \
	$(ALIB)(xendd.o) \
	$(ALIB)(xopenw.o) \
	$(ALIB)(xpng.o) \
	$(ALIB)(xsdatt.o) \
	$(ALIB)(xwrpng.o) \
	$(ALIB)(xxflsh.o)

XWOBJS = xclosp.o xendd.o xopenw.o xsdatt.o xxflsh.o

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
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(X11LIBDIR) -lX11 \
	$(SYSLIBS) \
	-lm

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) -I$(PNGINC)

all : pnglib $(PROG)

pnglib :
	@if [ ! -f $(ALIB) ] ; then \
	   $(CP) $(XW) $(ALIB) ; \
	   $(AR) -d $(ALIB) $(XWOBJS) ; \
	   $(COMPILE.c) x*.c ; \
	   $(AR) $(ARFLAGS) $(ALIB) *.o ; \
	fi

$(ALIB): $(LOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


$(PROG): $(ALIB)
	$(LN) $(GEMPAK)/source/device/main/device.f .
	$(LINK.f) device.f $(LIBINC) -o $@
	$(RM) device.o device.f

install:
	$(INSTALL) $(PROG) $(OS_BIN)

clean:
	$(RM) *.o device.f $(PROG)

