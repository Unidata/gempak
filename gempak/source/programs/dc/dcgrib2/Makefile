#
#	Makefile for GRIB decoder, "dcgrib2"
#
# Chiz                  11/99

include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(GEMLIB) \
	$(BRIDGE) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NAWIPS)/os/$(NA_OS)/lib/libjasper.a \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB)

PROG = dcgrib2

OBJS = \
	dcchecksub.o \
	dcfillgrid.o \
	dcflnam.o \
	dcflnam2.o \
	dcgfcyl.o \
	dcgrib.o \
	dcggrib.o \
	dcgnav.o \
	dcitoc.o \
	dcogfil.o \
	dcogfil2.o \
	dcsgrid.o \
	dcsubgrid.o \
	decode_grib.o \
	decode_grib1.o \
	decode_grib2.o \
	decode_g2gnum.o \
	dcquasi.o \
	dcwppg.o \
	stagger.o \
	stagger_fill.o \
	tblist.o \
	tbsgrid.o \
	qlin.o

#	dcmisjpg.o  not used currently

CFLAGS = $(COPT) -I$(GEMPAK)/source/griblib/gb -I$(GEMPAK)/source/bridge/dc -I$(GEMPAK)/source/griblib/na
FFLAGS = $(FOPT) -I$(GEMPAK)/source/bridge/dc -I$(GEMPAK)/source/gemlib/er

all : $(PROG)

$(PROG) : $(OBJS)
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(LIBS) $(SYSLIBS) -lm -o $@ 

install : 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) dcgrib2.hlp $(GEMHLP)/hlp

clean: testclean
	$(RM) $(OBJS) $(PROG) *.c~

test: 
	cat testdata/fh.0000_tl.press_gr.awp218 | dcgrib2 -v 1 -d -
	cat testdata/fh.0000_tl.press_gr.0p5deg | dcgrib2 -v 1 -d -
	cat testdata/2000011900_OCEAN.wmo | dcgrib2 -v 1 -d - 
	cat testdata/99120800_MRF.wmo | dcgrib2 -v 1 -d - 
	cat testdata/2000010412_ECMWF*.wmo | dcgrib2 -v 1 -d -
	cat testdata/2000012115_RUC.wmo | dcgrib2 -v 1 -d -
	cat testdata/2000010412_AVN.wmo | dcgrib2 -v 2 -d - 
#
#	cat testdata/2000011900_OCEAN.wmo | dcgrib2 -v 1 -d - YYYYMMDDHH_ocn.gem
#	cat testdata/99120800_MRF.wmo | dcgrib2 -v 1 -d - YYYYMMDDHH_mrf.gem
#	cat testdata/2000010412_AVN.wmo | dcgrib2 -v 1 -d - YYYYMMDDHH_thin.gem
#
#	cat testdata/99120800_ETA.wmo | dcgrib2 -v 1 -d - YYYYMMDDHH_eta@@@_###.gem
#	cat testdata/99120800_MRF.wmo | dcgrib2 -v 1 -d - YYYYMMDDHH_mrf@@@_###.gem

testclean:
	$(RM) 1999*.gem
	$(RM) 2000*.gem
	rm -rf data
