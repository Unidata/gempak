#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdp.a

COBJS = 

LOBJS = \
	$(ALIB)(testgdp.o) \

PROG = testgdp
PROG.f = testgdp.f
 

LIBINC = \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_NC = \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(GPLT_NC) \
	$(GEMLIB) \
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
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_GF = \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
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
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_GIF = \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
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
	$(LIBBZ2)

LIBINC_PS = \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/gdmap.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(PS) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

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
	$(LIBBZ2)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_GIF = $(PROG)_gif
PROG_XW = $(PROG)_xw
PROG_PS = $(PROG)_ps

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) -lm -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) -lm -o $(OS_BIN)/$@

$(PROG_PS) : $(ALIB) $(LIBINC_PS) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_PS) -lm -o $(OS_BIN)/$@

$(PROG_GIF) : $(ALIB) $(LIBINC_GIF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GIF) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_XW) : $(ALIB) $(LIBINC_XW) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_XW) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) -lm -o $(OS_BIN)/$@

programs_gf: $(PROG_GF)

programs_gif: $(PROG_GIF)

programs_xw: $(PROG_XW)

programs_nc: $(PROG_NC)

programs_ps: $(PROG_PS)

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

