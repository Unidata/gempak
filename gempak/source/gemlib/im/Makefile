#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(imbzsec.o) \
	$(ALIB)(imdump.o) \
	$(ALIB)(imgvtota.o) \
	$(ALIB)(imicmn.o) \
	$(ALIB)(imrnex2.o) \
	$(ALIB)(imnexz.o) \
	$(ALIB)(imaroff.o) \
	$(ALIB)(imrcdf.o) \
	$(ALIB)(imrcdf4.o) \
	$(ALIB)(imrgin.o) \
	$(ALIB)(imrqpe.o) \
	$(ALIB)(imrtbl.o) \
	$(ALIB)(imsbgn.o) \
	$(ALIB)(imwgin.o) \
	$(ALIB)(imnexbz.o) \
	$(ALIB)(imnexbzwmo.o) \
	$(ALIB)(latlon_nws.o)

FOBJS = \
	$(ALIB)(imar2gm.o) \
	$(ALIB)(imbtot.o) \
	$(ALIB)(imcalib.o) \
	$(ALIB)(imcalbar.o) \
	$(ALIB)(imcbar.o) \
	$(ALIB)(imchtb.o) \
	$(ALIB)(imdrop.o) \
	$(ALIB)(imgtmp.o) \
	$(ALIB)(imgi2gm.o) \
	$(ALIB)(imgihd.o) \
	$(ALIB)(imgm2gi.o) \
	$(ALIB)(imgossae4.o) \
	$(ALIB)(imisub.o) \
	$(ALIB)(imlabl.o) \
	$(ALIB)(imlutf.o) \
	$(ALIB)(imnex2.o) \
	$(ALIB)(imnidh.o) \
	$(ALIB)(imhrnidh.o) \
	$(ALIB)(imdpnidh.o) \
	$(ALIB)(imncdf4.o) \
	$(ALIB)(imnihd.o) \
	$(ALIB)(imnohd.o) \
	$(ALIB)(imqimg.o) \
	$(ALIB)(imqlut.o) \
	$(ALIB)(imqchn.o) \
	$(ALIB)(imqsiz.o) \
	$(ALIB)(imqtim.o) \
	$(ALIB)(imncdf.o) \
	$(ALIB)(imnzhd.o) \
	$(ALIB)(imrad2.o) \
	$(ALIB)(imsimg.o) \
	$(ALIB)(imsmgd.o) \
	$(ALIB)(imttob.o) \
	$(ALIB)(imunhd.o) \
	$(ALIB)(prnlnr.o) \
	$(ALIB)(prnlon.o)

 
GDIR = im

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) -I$(NETCDFINC) -I$(ZLIBINC) -I$(RSLINC)

all : $(ALIB) nids

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o

nids:
	( cd imnids;  echo -n "making in " ; \
		pwd ; $(MAKE) $(MFLAGS) all ; cd .. ) 


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GN) $(NETCDF) $(LIBJASPER) $(PNGLIB) $(ZLIB) $(RSLLIB) $(LIBBZ2) $(GEMLIB) $(GPLT)
	$(CP) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

tlink_xw : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR)_xw $(GPLT_NC) $(GEMLIB) $(CGEMLIB) $(GPLT) $(DEVICE) $(XW) $(GN) $(CGEMLIB) $(GEMLIB) $(NETCDF) $(LIBJASPER) $(PNGLIB) $(ZLIB) $(LIBINC_XW) $(X11LIBDIR) $(X11LIB) $(RSLLIB) $(LIBBZ2) $(SYSLIBS)
	$(CP) test$(GDIR)_xw $(OS_BIN)
	$(RM) *.o test$(GDIR)_xw

