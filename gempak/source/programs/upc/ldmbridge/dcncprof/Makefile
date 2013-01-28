#
#	Makefile for netcdf profiler/rass decoder, "dcncprof"
#
include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(BRIDGE) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LDMLOG) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(CSYSLIB) -lm 

####################

CFLAGS = $(COPT) -DGEMPAK -I$(NETCDFINC) -I$(GEMPAK)/source/programs/upc/ldmlog

PROG = dcncprof

PROG_OBJS =  \
	dcncprof.o \
	decode_ncprof.o \
	decode_rass.o \
	decode_fsl2.o \
	decode_fsl1.o \
	vapor_pres.o \
	write_gempak.o

all : $(PROG)

$(PROG) : $(PROG_OBJS)
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) -o $@

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o *.nts 1999*_prof.gem 2001*_prof.gem

reallyclean: clean
	rm -f $(PROG)

tarfile:
	tar cvf ../$(PROG).tar *
	compress ../$(PROG).tar

test: $(PROG)
	dcncprof -x -l - -f testdata/993030300.prof YYYYMMDDHH_prof.gem
	dcncprof -x -l - -f testdata/20011072006.nc YYYYMMDDHH_prof.gem
	dcncprof -x -l - -f testdata/20052792000.nc YYYYMMDDHH_prof.gem
