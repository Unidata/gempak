#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(cmpfil.o) \
	$(ALIB)(nexr_list_lev.o) \
	$(ALIB)(print_header_info.o)

LOBJS = \
	$(ALIB)(gpminp.o) \
	$(ALIB)(gpmopt.o) \
	$(ALIB)(gtmfls.o)

ALIB = $(OS_LIB)/gpnexr2.a
 
PROG = gpnexr2
PROG.f = gpnexr2.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(NETCDF) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(OS_LIB)/rsl.a \
        $(XMLLIB) \
        $(ZLIB) \
        $(LIBBZ2) \
        $(CGEMLIB)

LIBINC_PNG = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(OS_LIB)/png.a \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(OS_LIB)/rsl.a \
        $(XMLLIB) \
        $(ZLIB) \
        $(LIBBZ2) \
        $(CGEMLIB)

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GF) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)

LIBINC_XW = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XW) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)

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
        $(CGEMLIB)

LIBINC_PS = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(PS) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(OS_LIB)/rsl.a \
        $(XMLLIB) \
        $(ZLIB) \
        $(LIBBZ2) \
        $(CGEMLIB)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_PS = $(PROG)_ps
PROG_XW = $(PROG)_xw
PROG_PNG = $(PROG)_png

CFLAGS= $(COPT) -I$(RSLINC)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_ps : $(PROG_PS)

programs_gf : $(PROG_GF)

programs_nc : $(PROG_NC)

programs_xw : $(PROG_XW)

programs_png : $(PROG_PNG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(RSLLIB) $(LIBBZ2) -lm $(SYSLIBS) -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_PS) : $(ALIB) $(LIBINC_PS) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_PS) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_XW) : $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_XW) $(RSLLIB) $(LIBBZ2) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) $(RSLLIB) $(LIBBZ2) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GIF) $(RSLLIB) $(LIBBZ2) $(SYSLIBS) -o $(OS_BIN)/$(PROG)_gif

$(PROG_PNG) : $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_PNG) $(RSLLIB) $(LIBBZ2) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@

$(ALIB): $(LOBJS) $(COBJS)
	-$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG) $(ALIB)

help: $(PROG).txt
	hlpbld . $(PROG)

pdf: $(PROG).prm
	pdfbld . $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).pdf $(GEMPDF)
	$(AUXINSTALL) $(PROG).prm $(GEMPARM)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp
	$(AUXINSTALL) $(PROG).txt $(GEMPTXT)
	$(AUXINSTALL) $(PROG).err $(GEMERR)
	$(AUXINSTALL) tilt.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) tilt.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) radparm.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) radparm.hl2 $(GEMHLP)/hlx

dumplev2: print_header_info.c
	$(LINK.c) -DMAIN print_header_info.c $(RSLLIB) $(LIBBZ2) $(CGEMLIB) -lm -o $@
