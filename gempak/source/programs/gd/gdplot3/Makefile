#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdplot3.a
 
LIBINC =

COBJS = \
 	$(ALIB)(gdfill.o) \
 	$(ALIB)(gdpltc.o)

LOBJS = \
	$(ALIB)(gdanot.o) \
	$(ALIB)(gdbhlo.o) \
	$(ALIB)(gdbinp.o) \
	$(ALIB)(gdblev.o) \
	$(ALIB)(gdinst.o) \
	$(ALIB)(gdnarc.o) \
	$(ALIB)(gdnarw.o) \
	$(ALIB)(gdnbrb.o) \
	$(ALIB)(gdncrv.o) \
	$(ALIB)(gdnfnt.o) \
	$(ALIB)(gdnlin.o) \
	$(ALIB)(gdnply.o) \
	$(ALIB)(gdnreg.o) \
	$(ALIB)(gdnsym.o) \
	$(ALIB)(gdntxt.o) \
	$(ALIB)(gdpban.o) \
	$(ALIB)(gdpltb.o) \
	$(ALIB)(gdpltcf.o) \
	$(ALIB)(gdpprt.o) \
	$(ALIB)(gdpskp.o) \
	$(ALIB)(gdpstp.o) \
	$(ALIB)(gdpstt.o) \
	$(ALIB)(gdptmc.o) \
	$(ALIB)(gdptms.o) \
	$(ALIB)(gdptxt.o) \
	$(ALIB)(gdpwnd.o)


PROG = gdplot3
PROG.f = gdplot3.f

LIBINC = \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(OS_LIB)/sfmap.a \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_NC = \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(OS_LIB)/sfmap.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_GF = \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(OS_LIB)/sfmap.a \
	$(GPLT_NC) \
	$(GEMLIB) \
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
        $(GEMLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_GIF = \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(OS_LIB)/sfmap.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GIF) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_XW = \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XW) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_GIF = $(PROG)_gif
PROG_XW = $(PROG)_xw

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GIF) : $(ALIB) $(LIBINC_GIF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GIF) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_XW) : $(ALIB) $(LIBINC_XW) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_XW) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

programs_gf: $(PROG_GF)

programs_gif: $(PROG_GIF)

programs_xw: $(PROG_XW)

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

