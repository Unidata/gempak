#
#	Makefile for GEMPAK 5.2
#
#
include $(CONFIGDIR)/$(MAKEINC)

OLIB = $(OS_LIB)/gdtopo.a

PROG.f = gdtopo.f

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(PNGLIB) \
	$(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB)

FLIBMODS = \
	$(OLIB)(gdtdsp.o) \
	$(OLIB)(gdtfil.o) \
	$(OLIB)(gdtinp.o) \
	$(OLIB)(gdtwdt.o) \
	$(OLIB)(read_dem.o)

PROG = $(PROG.f:.f=)

all : $(PROG) 

$(PROG) : $(OLIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(OLIB) $(LIBINC) $(CSYSLIB) -lm -o $@

$(OLIB) : $(FLIBMODS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a:
	$(FC) $(FFLAGS) -c $<

.c.a:
	$(CC) $(CFLAGS) -c $<


install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) gdtopo.err $(GEMERR)
	$(AUXINSTALL) gdtopo.pdf $(GEMPDF)
	$(AUXINSTALL) gdtopo.prm $(GEMPARM)
	$(AUXINSTALL) gdtopo.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) gdtopo.txt $(GEMPTXT)
	$(AUXINSTALL) topofl.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) topofl.hl2 $(GEMHLP)/hlx

help: $(PROG).txt
	hlpbld . $(PROG)

pdf: $(PROG).prm
	pdfbld . $(PROG)

clean:
	rm -f $(PROG) $(OLIB) *.o

