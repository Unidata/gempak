#
#	Makefile for GEMPAK 5.2
#		Builds programs
#		Unidata 7/94 P.Bruehl
#
include $(CONFIGDIR)/$(MAKEINC)

PROG.f = gdpoint.f
PROG.o = gdpoint.o

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB)

OBJS = gdptin.o gdpttm.o gdptup.o gdpoint.o dgconav.o cwrapgcalc.o \
	cwrapgcalcf.o


PROG = $(PROG.f:.f=)

all : $(LIBINC) $(PROG)

$(PROG) : $(OBJS) $(LIBINC) $(PROG.f) 
	$(LINK.f) \
		$(OBJS) \
		$(LIBINC) \
		$(SYSLIBS) \
		-o $@

clean:
	$(RM) $(PROG) *.o

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).pdf $(GEMPDF)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp

