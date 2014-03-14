#
#	Makefile for netcdf ACARS decoder, "dcacars"
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
	$(NETCDF) \
	$(CSYSLIB) -lm

####################

PROG = dcacars

PROG_OBJS =  \
	dcacars.o \
	decode_acars.o \
	decode_ncacars.o \
	vapor_pres.o \
	write_gempak.o

CFLAGS = $(COPT) -DGEMPAK -I$(NETCDFINC) -I$(GEMPAK)/source/programs/upc/ldmlog
FFLAGS = $(FOPT) -I$(GEMPAK)/source/gemlib/sf

all : $(PROG) 
	
$(PROG) : $(PROG_OBJS)
	@case $(NA_OS) in \
	osf) \
	$(LINK.c) $(PROG_OBJS) $(LIBS) $(CSYSLIB) -lUfor -lfor -lots -o $@ ;; \
	*) \
	$(LINK.f) $(NOFORMAIN) $(PROG_OBJS) $(LIBS) -o $@ ;; \
	esac

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp

clean:
	rm -f $(PROG_OBJS) $(PROG) *~ *.o *.nts 99020914_acars.gem

reallyclean: clean
	rm -f $(PROG)

tarfile:
	tar cvf ../$(PROG).tar *
	compress ../$(PROG).tar

test:
	cat 9902091400_acars.nc | dcacars -vl - YYMMDDHH_acars.gem
