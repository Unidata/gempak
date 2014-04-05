#
#	Makefile for GEMPAK 6.0
#		Builds GDRADR
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gpnids.a

PROG = gpnids
PROG.f = $(PROG).f


FOBJS = \
	$(ALIB)(gplutf.o) \
	$(ALIB)(gpvinp.o) \
	$(ALIB)(gtmfls.o) \
	$(ALIB)(text_output.o)

COBJS = \
	$(ALIB)(cmpfil.o)

#	\
#	$(ALIB)(gdctbl.o) \
#	$(ALIB)(packet_codes.o) \
#	$(ALIB)(read_vad.o) \
#	$(ALIB)(vad_driver.o)
#	$(ALIB)(vad_head.o) \
#	$(ALIB)(vad_symbol.o) \
#	$(ALIB)(vad_tabular.o) \
#	$(ALIB)(vad_alpha_graphic.o) \
#	$(ALIB)(xytolatlon.o)

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GF) \
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

LIBINC_GIF = \
        $(GPLT_NC) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GPLT) \
        $(DEVICE) \
        $(GIF) \
        $(GN) \
        $(CGEMLIB) \
        $(GPLT) \
        $(GEMLIB) \
        $(NETCDF) \
        $(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_VG = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(VG) \
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

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_VG = $(PROG)_vg

CFLAGS = $(COPT) -DDEBUG

all : $(ALIB) $(PROG)

programs_gf: $(PROG_GF) $(PROG_VG)
programs_vg: $(PROG_VG)

$(PROG) : $(LIBINC) $(ALIB) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_GF) : $(LIBINC_GF) $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GIF) $(RSLLIB) $(LIBBZ2) $(SYSLIBS) -o $(OS_BIN)/$(PROG)_gif

$(PROG_VG) : $(LIBINC_VG) $(ALIB) $(PROG.f)
	$(COMPILE.f) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_VG) $(SYSLIBS) -o $(OS_BIN)/$@
	$(RM) $(PROG).o

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o $(ALIB)


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $?.pdf $(GEMPDF)
	$(AUXINSTALL) $?.prm $(GEMPARM)
	$(AUXINSTALL) $?.err $(GEMERR)
	$(AUXINSTALL) $?.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) $?.txt $(GEMPTXT)
