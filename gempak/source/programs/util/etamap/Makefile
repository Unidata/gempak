#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

FOBJS = \
	$(ALIB)(etainp.o) \
	$(ALIB)(etaopt.o)

ALIB = $(OS_LIB)/etamap.a
 
PROG = etamap
PROG.f = etamap.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(NETCDF) \
        $(XMLLIB) \
        $(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(GPLT) \
        $(XMLLIB) \
        $(ZLIB) \
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
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(GPLT) \
        $(XMLLIB) \
        $(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_PS = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(PS) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_PS = $(PROG)_ps

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_ps : $(PROG_PS)

programs_gf : $(PROG_GF)

programs_nc : $(PROG_NC)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_PS) : $(ALIB) $(LIBINC_PS) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_PS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(ALIB): $(FOBJS) $(COBJS)
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
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp
	$(CP) *.hl1 $(GEMHLP)/hlx
	$(CP) *.hl2 $(GEMHLP)/hlx
