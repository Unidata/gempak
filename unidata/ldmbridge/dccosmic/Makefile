#
#	Makefile for netcdf COSMIC decoder, "dccosmic"
#
include $(CONFIGDIR)/$(MAKEINC)

LIBS = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(BRIDGE) \
	$(LDMLOG) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB) \
	$(NETCDF) \
	$(CSYSLIB) -lm

####################

PROG = dccosmic

PROG_OBJS =  \
	dccosmic.o \
	decode_cosmic.o \
	decode_nccosmic.o \
	vapor_pres.o \
	write_gempak.o

CFLAGS = $(COPT) -DGEMPAK -I$(NETCDFINC) -I$(NAWIPS)/unidata/ldmlog
FFLAGS = $(FOPT) -I$(GEMPAK)/source/gemlib/sf

all : $(PROG) 
	
$(PROG) : $(PROG_OBJS)
	@case $(NA_OS) in \
	osf) \
	$(LINK.c) $(PROG_OBJS) $(LIBS) -lUfor -lfor -lots -lm -o $@ ;; \
	*) \
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) -o $@ ;; \
	esac

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp
	$(AUXINSTALL) *.pack $(GEMTBL)/pack
	$(AUXINSTALL) cosmic_decode.csh $(OS_BIN)

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o *.nts 20*_cosmic.gem

reallyclean: clean
	rm -f $(PROG)

tarfile:
	tar cvf ../$(PROG).tar *
	compress ../$(PROG).tar

test:
	cat wetPrf_C003.2007.240.15.04.G09_0001.0002_nc | dccosmic -vl - YYYYMMDDHHNN_cosmic.gem
