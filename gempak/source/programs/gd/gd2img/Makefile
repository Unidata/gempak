#
#	Makefile for GEMPAK 5.2
#		Builds programs
#		Unidata 7/94 P.Bruehl
#
include $(CONFIGDIR)/$(MAKEINC)

PROG.f = gd2img.f
PROG.o = gd2img.o

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

OBJS = gdptin.o gdpttm.o gdptup.o gd2img.o dgconav.o gdcnav.o gdcanl.o \
	gdlgin.o writepng.o

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


install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).pdf $(GEMPDF)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp

