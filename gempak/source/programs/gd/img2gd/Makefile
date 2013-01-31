#
#	Makefile for GEMPAK 6.0
#		Builds IMG2GD
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/img2gd.a

PROG.f = img2gd.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gdcanl.o) \
	$(ALIB)(gdcnav.o) \
	$(ALIB)(gdctbl.o)

COBJS = \
	$(ALIB)(radar_grid.o) \
	$(ALIB)(waittime.o)

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GIF) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

PROG = $(PROG.f:.f=)
PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)

all : $(ALIB) $(PROG)

$(PROG) : $(ALIB) $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o $(ALIB)

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) img2gd.pdf $(GEMPDF)
	$(AUXINSTALL) img2gd.prm $(GEMPARM)
	$(AUXINSTALL) img2gd.err $(GEMERR)
	$(AUXINSTALL) img2gd.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) img2gd.txt $(GEMPTXT)
	$(AUXINSTALL) calimg.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) calimg.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) imgtim.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) imgtim.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) imgfil.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) imgfil.hl2 $(GEMHLP)/hlx

help: $(PROG).txt
	hlpbld . $(PROG)

pdf: $(PROG).prm
	pdfbld . $(PROG)
