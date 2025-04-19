#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(oagagn.o) \
	$(ALIB)(oagspc.o) \
	$(ALIB)(sfmclr.o) \
	$(ALIB)(sfmcof.o) \
	$(ALIB)(sfmdat.o) \
	$(ALIB)(sfmfil.o) \
	$(ALIB)(sfminp.o) \
	$(ALIB)(sfmopt.o) \
	$(ALIB)(sfmovr.o) \
	$(ALIB)(sfmplt.o) \
	$(ALIB)(sfmprm.o)
 
ALIB = $(OS_LIB)/sfcntr.a
 
PROG = sfcntr
PROG.f = sfcntr.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
	$(ZLIB) \
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
	$(CGEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
	$(ZLIB) \
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
	$(CGEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

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
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $?.err $(GEMERR)
	$(AUXINSTALL) $?.prm $(GEMPARM)
	$(AUXINSTALL) $?.pdf $(GEMPDF)
	$(AUXINSTALL) $?.txt $(GEMPTXT)
	$(AUXINSTALL) $?.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) *.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) *.hl2 $(GEMHLP)/hlx
