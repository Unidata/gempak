#
#	Makefile for GEMPAK 6.0
#		Builds programs
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/acprof.a

PROG.f = acprof.f

FOBJS = \
	$(ALIB)(mknam.o) \
	$(ALIB)(snpbck.o) \
	$(ALIB)(snpdat.o) \
	$(ALIB)(snpdtw.o) \
	$(ALIB)(acpfil.o) \
	$(ALIB)(snpgrf.o) \
	$(ALIB)(snpinp.o) \
	$(ALIB)(snplin.o) \
	$(ALIB)(snppln.o) \
	$(ALIB)(snpplt.o) \
	$(ALIB)(snpprm.o) \
	$(ALIB)(snppst.o) \
	$(ALIB)(snpstb.o) \
	$(ALIB)(snpxax.o) \
	$(ALIB)(snpyax.o)

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
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
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)

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
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)

PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)
PROG = $(PROG.f:.f=)

all : $(ALIB) $(PROG)

$(PROG) : $(ALIB) $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

programs_nc : $(PROG_NC)

programs_gf : $(PROG_GF)

$(PROG_NC) : $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_GF) : $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -o $(OS_BIN)/$@

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) $(ALIB) *.o *.nts


install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) $?.pdf $(GEMPDF)
	$(AUXINSTALL) $?.prm $(GEMPARM)
	$(AUXINSTALL) $?.err $(GEMERR)
	$(AUXINSTALL) $?.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) $?.txt $(GEMPTXT)
