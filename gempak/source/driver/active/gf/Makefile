#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = gf
 
ALIB = $(OS_LIB)/gf.a
 
DEVLIBS = $(GN)

LOBJS = \
	$(ALIB)(hclosp.o) \
	$(ALIB)(henanm.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(heplot.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hslwin.o) \
	$(ALIB)(hstanm.o) \
	$(ALIB)(gfclosp.o) \
	$(ALIB)(gfdatt.o) \
	$(ALIB)(gfendd.o) \
	$(ALIB)(gfflsh.o) \
	$(ALIB)(gfinita.o) \
	$(ALIB)(gfopenw.o) \
	$(ALIB)(gfselwin.o) \
	$(ALIB)(xwrgif.o) \

XWOBJS = hclosp.o henanm.o hendd.o heplot.o hinita.o hsdatt.o hslwin.o hstanm.o xclosp.o xendd.o xinita.o xopenw.o xsdatt.o

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
	$(GEMLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(DEVLIBS) \
	$(CGEMLIB) \
	$(X11LIBDIR) -lX11 \
	$(SYSLIBS) \
	-lm

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : gflib $(PROG)

gflib :
	@if [ ! -f $(GF) ] ; then \
	   echo "copying xw.a to gf.a"; \
	   $(CP) $(XW) $(ALIB); sleep 1; \
	   echo "removing xwobjs"; \
	   $(AR) -d $(ALIB) $(XWOBJS) ; \
	   echo "compiling c source"; \
	   $(COMPILE.c) *.c ; \
	   echo "adding objects to gf.a"; \
	   $(AR) $(ARFLAGS) $(ALIB) *.o ; \
	fi

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o

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

