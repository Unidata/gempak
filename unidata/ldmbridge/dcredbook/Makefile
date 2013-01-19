#
#	Makefile for REDBOOK graphics decoder, "dcredbook"
#
# Chiz                  8/00

include $(CONFIGDIR)/$(MAKEINC)

DCLIBS = $(BRIDGE)
GFDEVLIBS = $(GPLT) $(DEVICE) $(GF) $(GN)
GIFDEVLIBS = $(GPLT) $(DEVICE) $(GIF) $(GN)
PSDEVLIBS = $(GPLT) $(DEVICE) $(PS) $(GN)
VGDEVLIBS = $(GPLT) $(DEVICE) $(VG) $(GN) \
	   $(OS_LIB)/libxslt.a $(ZLIB)

GFLIBS = $(GPLT_NC) $(GEMLIB) $(CGEMLIB) $(GFDEVLIBS) $(DCLIBS) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(GPLT) $(NETCDF) $(PNGLIB) $(ZLIB) $(RSLLIB) $(LIBBZ2) $(X11LIBDIR) $(X11LIB) $(XW) $(GN)

GIFLIBS = $(GPLT_NC) $(GEMLIB) $(CGEMLIB) $(GIFDEVLIBS) $(DCLIBS) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(GPLT) $(NETCDF) $(PNGLIB) $(ZLIB) $(RSLLIB) $(LIBBZ2)

PSLIBS = $(GPLT_NC) $(GEMLIB) $(CGEMLIB) $(PSDEVLIBS) $(DCLIBS) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(GPLT) $(NETCDF) $(PNGLIB) $(ZLIB) $(RSLLIB) $(LIBBZ2)

VGLIBS = $(GPLT_NC) $(GEMLIB) $(CGEMLIB) $(VGDEVLIBS) $(DCLIBS) $(GEMLIB) $(CGEMLIB) $(GEMLIB) $(GPLT) $(NETCDF) $(PNGLIB) $(ZLIB) $(RSLLIB) $(LIBBZ2)

LIBS = $(GEMLIB) $(APPL) $(SYSLIB) $(DCLIBS) $(CGEMLIB) $(GEMLIB) $(CGEMLIB) $(NETCDF) $(ZLIB) $(RSLLIB) $(LIBBZ2) $(XW)

PROG = dcredbook

OBJS = \
	ipredbook.o \
	redbook_header.o \
	decode_redbook.o \
	dcredbook.o

CFLAGS = $(COPT) -I$(GEMPAK)/source/bridge/dc
FFLAGS = $(FOPT) -I$(GEMPAK)/source/bridge/dc -I$(GEMPAK)/source/gemlib/ip

all : $(PROG)

$(PROG) : $(OBJS) $(PROG).o
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(LIBS) $(SYSLIBS) $(TEXTLIBS) -lm -o $@ 
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(GFLIBS) $(SYSLIBS) $(TEXTLIBS) -lm -o $@_gf
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(GIFLIBS) $(SYSLIBS) $(TEXTLIBS) -lm -o $@_gif
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(PSLIBS) $(SYSLIBS) $(TEXTLIBS) -lm -o $@_ps
	$(LINK.f) $(NOFORMAIN) $(OBJS) $(VGLIBS) $(SYSLIBS) $(TEXTLIBS) -lm -o $@_vg

install : 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(INSTALL) $(PROG)_gf $(OS_BIN)
	$(INSTALL) $(PROG)_gif $(OS_BIN)
	$(INSTALL) $(PROG)_ps $(OS_BIN)
	$(INSTALL) $(PROG)_vg $(OS_BIN)
	$(AUXINSTALL) $(PROG).hlp	$(GEMHLP)/hlp

clean:
	$(RM) $(OBJS) $(PROG) $(PROG)_gf $(PROG)_gif $(PROG)_ps $(PROG)_vg 96P-*test.ps

test: 
	cat 96P.rbk | dcredbook_ps -d - 'PS|%P_test.ps'
