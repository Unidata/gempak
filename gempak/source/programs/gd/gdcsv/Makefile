#
#	Makefile for GEMPAK 5.2
#		Builds programs
#		Unidata 7/94 P.Bruehl
#
include $(CONFIGDIR)/$(MAKEINC)

PROG.f = gdcsv.f
PROG.o = gdcsv.o

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

OBJS = gdptin.o gdpttm.o gdcsv.o gdpout.o dgconav.o \
	cwrapgcalc.o cwrapgcalcf.o


PROG = $(PROG.f:.f=)

all : $(LIBINC) $(PROG)

$(PROG) : $(OBJS) $(LIBINC) $(PROG.f) 
	$(LINK.f) \
		$(OBJS) \
		$(LIBINC) \
		$(SYSLIBS) \
		-o $@
programs_gf:
	@echo "Not a graphic program, no _gf program needed"

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

clean:
	$(RM) $(PROG) *.o


install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	-$(AUXINSTALL) $(PROG).pdf $(GEMPDF)
	-$(AUXINSTALL) $(PROG).prm $(GEMPARM)
	-$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp

