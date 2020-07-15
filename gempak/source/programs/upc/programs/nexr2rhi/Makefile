#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(cmpfil.o) \
	$(ALIB)(rnex2.o) \
	$(ALIB)(rebin_velocity.o)

LOBJS = \
	$(ALIB)(gdxdsp.o) \
	$(ALIB)(gdxinp.o) \
	$(ALIB)(gdxlev.o) \
	$(ALIB)(gdxplt.o) \
	$(ALIB)(gdxsug.o) \
	$(ALIB)(gdxyax.o) \
	$(ALIB)(gtmfls.o) \
	$(ALIB)(setgproj.o)

ALIB = $(OS_LIB)/nexr2rhi.a
 
PROG = nexr2rhi
PROG.f = nexr2rhi.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(SYSLIB) \
	$(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GF) \
	$(GN) \
	$(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_XW = \
	$(GPLT_NC) \
	$(GEMLIB) \
        $(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XW) \
	$(GN) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_XW = $(PROG)_xw

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) -I$(RSLINC)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_XW) : $(ALIB) $(LIBINC_XW) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_XW) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

programs_xw: $(PROG_XW)

programs_gf: $(PROG_GF)

programs_nc: $(PROG_NC)

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG) $(ALIB)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $(PROG).pdf $(GEMPDF)
	$(AUXINSTALL) $(PROG).prm $(GEMPARM)
	$(AUXINSTALL) $(PROG).err $(GEMERR)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp
	$(AUXINSTALL) $(PROG).txt $(GEMPTXT)
	$(AUXINSTALL) interp.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) interp.hl2 $(GEMHLP)/hlx
