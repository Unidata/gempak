#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gpscat.a

PROG = gpscat
PROG.f = gpscat.f

LOBJS =	\
	$(ALIB)(gpminp.o) \
	$(ALIB)(gpmopt.o)

LIBINC =	\
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
        $(DALIB) \
	$(CGEMLIB) \
	$(LIBGPC) \
	$(NETCDF) \
	$(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_NC = \
	$(GPLT_NC) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GPLT) \
        $(DEVICE) \
        $(NC) \
        $(GN) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(DALIB) \
        $(LIBGPC) \
        $(NETCDF) \
        $(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
        $(PNGLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2)

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
        $(GEMLIB) \
        $(DALIB) \
        $(LIBGPC) \
        $(NETCDF) \
        $(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
        $(PNGLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2)

LIBINC_XW = \
        $(GPLT_NC) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GPLT) \
        $(DEVICE) \
        $(XW) \
        $(GN) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(DALIB) \
        $(LIBGPC) \
        $(NETCDF) \
        $(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
        $(PNGLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2)

LIBINC_GIF = \
        $(GPLT_NC) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GPLT) \
        $(DEVICE) \
        $(GIF) \
        $(GN) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(DALIB) \
        $(LIBGPC) \
        $(NETCDF) \
        $(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
        $(PNGLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2)

LIBINC_PS = \
        $(GPLT_NC) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GPLT) \
        $(DEVICE) \
        $(PS) \
        $(GN) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(DALIB) \
        $(LIBGPC) \
        $(NETCDF) \
        $(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
        $(PNGLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2)

LIBINC_VG = \
        $(GPLT_NC) \
        $(GEMLIB) \
        $(CGEMLIB) \
        $(GPLT) \
        $(DEVICE) \
        $(VG) \
        $(GN) \
        $(CGEMLIB) \
        $(GEMLIB) \
        $(DALIB) \
        $(LIBGPC) \
        $(NETCDF) \
        $(OS_LIB)/libmel_bufr.a \
        $(XMLLIB) \
        $(PNGLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2)

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf
PROG_PS = $(PROG)_ps
PROG_VG = $(PROG)_vg
PROG_XW = $(PROG)_xw

 


GDIR = utf

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_ps : $(PROG_PS)

programs_gf : $(PROG_GF)

programs_nc : $(PROG_NC)

programs_xw : $(PROG_XW)

programs_vg : $(PROG_VG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_PS) : $(ALIB) $(LIBINC_PS) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_PS) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_VG) : $(ALIB) $(LIBINC_VG) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_VG) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_XW) : $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_XW) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -o $(OS_BIN)/$@
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GIF) $(SYSLIBS) -o $(OS_BIN)/$(PROG)_gif

$(ALIB): $(LOBJS) 
	-$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG) 

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) $(NOFORMAIN) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

