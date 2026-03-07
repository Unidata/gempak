#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(snsbor.o) \
	$(ALIB)(snsckd.o) \
	$(ALIB)(snsdat.o) \
	$(ALIB)(snsdsp.o) \
	$(ALIB)(snsfil.o) \
	$(ALIB)(snsgrd.o) \
	$(ALIB)(snsgth.o) \
	$(ALIB)(snsinp.o) \
	$(ALIB)(snsins.o) \
	$(ALIB)(snsisn.o) \
	$(ALIB)(snslev.o) \
	$(ALIB)(snsprm.o) \
	$(ALIB)(snspts.o) \
	$(ALIB)(snsrec.o) \
	$(ALIB)(snsrge.o) \
	$(ALIB)(snssst.o) \
	$(ALIB)(snsstm.o) \
	$(ALIB)(snssur.o) \
	$(ALIB)(snstan.o) \
	$(ALIB)(snstha.o) \
	$(ALIB)(snsttl.o) \
	$(ALIB)(snsunt.o) \
	$(ALIB)(snswnd.o) \
	$(ALIB)(snswwe.o) \
	$(ALIB)(snsxax.o) \
	$(ALIB)(snsyax.o)

ALIB = $(OS_LIB)/sncross.a
 
PROG = sncross
PROG.f = sncross.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

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
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
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
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(LIBREADLINE)


PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@

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

