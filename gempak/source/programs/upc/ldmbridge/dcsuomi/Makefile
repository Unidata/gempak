#
#	Makefile for netcdf SUOMINET decoder, "dcuomicars"
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

PROG = dcsuomi

PROG_OBJS =  \
	dcsuomi.o \
	decode_suomi.o \
	decode_ncsuomi.o \
	write_gempak.o

CFLAGS = $(COPT) -DGEMPAK -I$(NETCDFINC) -I$(GEMPAK)/source/programs/upc/ldmlog
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
	$(AUXINSTALL) suomi.pack $(GEMTBL)/pack
	$(AUXINSTALL) suomi.tbl $(GEMTBL)/stns

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o *.nts 20*_suomi.gem

reallyclean: clean
	rm -f $(PROG)

tarfile:
	tar cvf ../$(PROG).tar *
	compress ../$(PROG).tar

test:
	cat suoPWV_2002.044.00.00.1440_nc | dcsuomi -vd - YYYYMMDD_suomi.gem
	cat suoHWV_2002.080.20.00.0060_nc | dcsuomi -d - YYYYMMDD_suomi.gem
	cat CsuPWVh_2007.095.23.00.0060_nc | dcsuomi -vd - YYYYMMDD_suomi.gem
