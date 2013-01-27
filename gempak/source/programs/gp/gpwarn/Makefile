#
#	Makefile for GEMPAK 6.0
#		Builds programs
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gpwarn.a

PROG.f = gpwarn.f

FOBJS = \
	$(ALIB)(ggsls.o) \
	$(ALIB)(ggwsls.o) \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gpmopt.o) \
	$(ALIB)(text_output.o)

COBJS = \
	$(ALIB)(read_bull.o) \
	$(ALIB)(plot_ugc.o)

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
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

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
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
	$(LIBBZ2)

PROG = $(PROG.f:.f=)
PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)

CFLAGS = $(COPT) $(GEMINC) -I$(GEMPAK)/source/cgemlib/clo

all : $(ALIB) $(PROG)

$(PROG) : $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

programs_nc : $(PROG_NC)

programs_gf : $(PROG_GF)

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

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
