#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	aodt.o \
	aodtv64drive.o \
	aodtv64runautomode.o \
	aodtv72drive.o \
	aodtv72runautomode.o

LOBJS = 
 
ALIB = 
 
PROGS = aodt

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(OS_LIB)/libAODT_v64.a \
	$(OS_LIB)/libAODT_v72.a \
	$(XW) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(RSLLIB) \
        $(LIBBZ2) \
	$(X11LIBDIR) $(X11LIB) -lm

CFLAGS = $(COPT) -I$(NAWIPS)/extlibs/AODT

all : $(PROGS)

aodt: $(COBJS)
	$(LINK.f) $(NOFORMAIN) $(COBJS) $(LIBINC) $(SYSLIBS) -o $@

clean:
	$(RM) *.o $(PROGS)

install: $(PROGS) 
	$(INSTALL) aodt $(OS_BIN)
