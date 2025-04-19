#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(sfxchr.o) \
	$(ALIB)(sfxcnd.o) \
	$(ALIB)(sfxdat.o) \
	$(ALIB)(sfxdsp.o) \
	$(ALIB)(sfxdta.o) \
	$(ALIB)(sfxfil.o) \
	$(ALIB)(sfxgrf.o) \
	$(ALIB)(sfxgst.o) \
	$(ALIB)(sfxinp.o) \
	$(ALIB)(sfxloc.o) \
	$(ALIB)(sfxplt.o) \
	$(ALIB)(sfxpnl.o) \
	$(ALIB)(sfxprm.o) \
	$(ALIB)(sfxrel.o) \
	$(ALIB)(sfxrng.o) \
	$(ALIB)(sfxsky.o) \
	$(ALIB)(sfxsym.o) \
	$(ALIB)(sfxttl.o) \
	$(ALIB)(sfxwnd.o) \
	$(ALIB)(sfxxax.o)
 
ALIB = $(OS_LIB)/sfgram.a
 
PROG = sfgram
PROG.f = sfgram.f
 

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
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -o $(OS_BIN)/$@

programs_nc: $(PROG_NC)

programs_gf: $(PROG_GF)

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

