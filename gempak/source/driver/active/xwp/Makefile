#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = xwp
 
ALIB = $(OS_LIB)/xwp.a
 
DEVLIBS = $(XW) $(PS) $(GN)
NEEDX11 = $(X11LIBDIR) $(X11LIB)
 
LOBJS = \
	$(ALIB)(hcirc.o) \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hclpnl.o) \
	$(ALIB)(henanm.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(heplot.o) \
	$(ALIB)(hevent.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hgtpnt.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hinitps.o) \
	$(ALIB)(hinitxw.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hloopc.o) \
	$(ALIB)(hqclrs.o) \
	$(ALIB)(hqcomp.o) \
	$(ALIB)(hqdatt.o) \
	$(ALIB)(hroam.o) \
	$(ALIB)(hsatim.o) \
	$(ALIB)(hsbrgb.o) \
	$(ALIB)(hscint.o) \
	$(ALIB)(hscnam.o) \
	$(ALIB)(hscolb.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hscrgb.o) \
	$(ALIB)(hsctbl.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hsicmn.o) \
	$(ALIB)(hslwid.o) \
	$(ALIB)(hslwin.o) \
	$(ALIB)(hsplot.o) \
	$(ALIB)(hsroam.o) \
	$(ALIB)(hstanm.o) \
	$(ALIB)(hstext.o) \
	$(ALIB)(htext.o) \
	$(ALIB)(htextc.o) \
	$(ALIB)(pinita.o)

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
	$(NEEDX11) \
        $(XMLLIB) \
        $(ZLIB) \
        $(SYSLIBS) \
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

