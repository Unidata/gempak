#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(gdxdta.o) \
	$(ALIB)(gdxdvv.o) \
	$(ALIB)(gdxgts.o)

LOBJS = \
	$(ALIB)(gdxcla.o) \
	$(ALIB)(gdxdsp.o) \
	$(ALIB)(gdxdtaf.o) \
	$(ALIB)(gdxdtv.o) \
	$(ALIB)(gdxdvvf.o) \
	$(ALIB)(gdxdxy.o) \
	$(ALIB)(gdxgrd.o) \
	$(ALIB)(gdxgtsf.o) \
	$(ALIB)(gdxinp.o) \
	$(ALIB)(gdxlen.o) \
	$(ALIB)(gdxlev.o) \
	$(ALIB)(gdxplt.o) \
	$(ALIB)(gdxpuw.o) \
	$(ALIB)(gdxscv.o) \
	$(ALIB)(gdxsdl.o) \
	$(ALIB)(gdxsfm.o) \
	$(ALIB)(gdxsug.o) \
	$(ALIB)(gdxyax.o)

ALIB = $(OS_LIB)/gdcross.a
 
PROG = gdcross
PROG.f = gdcross.f
 

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
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(GEMLIB) \
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
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(GEMLIB) \
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

