#
#	Makefile for GEMPAK 6.0
#		Builds GDRADR
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/vad.a

PROG = gpvad
PROG.f = $(PROG).f


FOBJS = \
	$(ALIB)(gpvinp.o) \
	$(ALIB)(gtmfls.o)

COBJS = \
	$(ALIB)(cmpfil.o)

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
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

all : $(ALIB) $(PROG)

programs_gf: $(PROG_GF)

$(PROG) : $(LIBINC) $(ALIB) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_GF) : $(LIBINC_GF) $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@

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
