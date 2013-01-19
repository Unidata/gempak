#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)
 
ALIB = $(OS_LIB)/sncross2.a

FSRC = sncwnd.f snswnd.f

COMMON_OBJS = \
	snsbor.o \
	snsckd.o \
	snsdat.o \
	snsdsp.o \
	snsfil.o \
	snsgrd.o \
	snsgth.o \
	snsinp.o \
	snsins.o \
	snsisn.o \
	snslev.o \
	snsprm.o \
	snspts.o \
	snsrec.o \
	snsrge.o \
	snssst.o \
	snsstm.o \
	snssur.o \
	snstan.o \
	snstha.o \
	snsttl.o \
	snsunt.o \
	snswwe.o \
	snsxax.o \
	snsyax.o

COMMON = \
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
	$(ALIB)(snswwe.o) \
	$(ALIB)(snsxax.o) \
	$(ALIB)(snsyax.o)

PROG = sncross2
PROG.f = sncross2.f
 

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
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(FSRC) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(FSRC) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(FSRC) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@

programs_gf: $(PROG_GF)

programs_nc: $(PROG_NC)

$(ALIB): $(COMMON_OBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

$(COMMON_OBJS) :
	@$(AR) -x $(OS_LIB)/sncross.a $@

clean:
	$(RM) *.o $(PROG) $(ALIB)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

