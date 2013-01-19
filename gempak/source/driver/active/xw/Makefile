#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = xw
 
ALIB = $(OS_LIB)/xw.a
 
DEVLIBS = $(GN)
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
	$(ALIB)(hgsave.o) \
	$(ALIB)(hgtpnt.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hloopc.o) \
	$(ALIB)(hqclrs.o) \
	$(ALIB)(hqcomp.o) \
	$(ALIB)(hqdatt.o) \
	$(ALIB)(hroam.o) \
	$(ALIB)(hsatim.o) \
	$(ALIB)(hsatpx.o) \
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
	$(ALIB)(xaiclr.o) \
	$(ALIB)(xarea.o) \
	$(ALIB)(xcaloc.o) \
	$(ALIB)(xcamgr.o) \
	$(ALIB)(xcirc.o) \
	$(ALIB)(xclear.o) \
	$(ALIB)(xclosp.o) \
	$(ALIB)(xclpnl.o) \
	$(ALIB)(xclrpxm.o) \
	$(ALIB)(xcpypxm.o) \
	$(ALIB)(xcpypxm2.o) \
	$(ALIB)(xcsdat.o) \
	$(ALIB)(xdot.o) \
	$(ALIB)(xdwtbl.o) \
	$(ALIB)(xenanm.o) \
	$(ALIB)(xendd.o) \
	$(ALIB)(xfill.o) \
	$(ALIB)(xg2pxm.o) \
	$(ALIB)(xgbank.o) \
	$(ALIB)(xginit.o) \
	$(ALIB)(xgsave.o) \
	$(ALIB)(xgsdat.o) \
	$(ALIB)(xgtoff.o) \
	$(ALIB)(xgtpnt.o) \
	$(ALIB)(xinita.o) \
	$(ALIB)(xinitclr.o) \
	$(ALIB)(xline.o) \
	$(ALIB)(xloopc.o) \
	$(ALIB)(xmexpo.o) \
	$(ALIB)(xmfrmtg.o) \
	$(ALIB)(xmloop.o) \
	$(ALIB)(xmotifw.o) \
	$(ALIB)(xmroam.o) \
	$(ALIB)(xopenw.o) \
	$(ALIB)(xpgpaste.o) \
	$(ALIB)(xpgrestlp.o) \
	$(ALIB)(xpgrfrsh.o) \
	$(ALIB)(xpgsetpg.o) \
	$(ALIB)(xpgsetlayer.o) \
	$(ALIB)(xpgsvfrm.o) \
	$(ALIB)(xpgsvfrm2.o) \
	$(ALIB)(xpgsvlp.o) \
	$(ALIB)(xpoint.o) \
	$(ALIB)(xputpxms.o) \
	$(ALIB)(xpxm2win.o) \
	$(ALIB)(xqclrs.o) \
	$(ALIB)(xqcmps.o) \
	$(ALIB)(xqcolr.o) \
	$(ALIB)(xqcomp.o) \
	$(ALIB)(xqcpxm.o) \
	$(ALIB)(xqdatt.o) \
	$(ALIB)(xqgemgc.o) \
	$(ALIB)(xqnclr.o) \
	$(ALIB)(xqpxms.o) \
	$(ALIB)(xrbpxm.o) \
	$(ALIB)(xroam.o) \
	$(ALIB)(xsatim.o) \
	$(ALIB)(xsatpx.o) \
	$(ALIB)(xscint.o) \
	$(ALIB)(xscnam.o) \
	$(ALIB)(xscolr.o) \
	$(ALIB)(xscpxm.o) \
	$(ALIB)(xscrgb.o) \
	$(ALIB)(xsdatt.o) \
	$(ALIB)(xselwin.o) \
	$(ALIB)(xsetver.o) \
	$(ALIB)(xsfill.o) \
	$(ALIB)(xsicmn.o) \
	$(ALIB)(xsincr.o) \
	$(ALIB)(xslwid.o) \
	$(ALIB)(xslwin.o) \
	$(ALIB)(xsncolr.o) \
	$(ALIB)(xsplot.o) \
	$(ALIB)(xsroam.o) \
	$(ALIB)(xstanm.o) \
	$(ALIB)(xstext.o) \
	$(ALIB)(xtext.o) \
	$(ALIB)(xtextc.o) \
	$(ALIB)(xupdclr.o) \
	$(ALIB)(xvgifwr.o) \
	$(ALIB)(xwgxor.o) \
	$(ALIB)(xxevnt.o) \
	$(ALIB)(xxflsh.o)

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
	$(NEEDX11) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(GEMLIB) \
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

