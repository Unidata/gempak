#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	nodt.o \
	odtcoakley.o \
	odtintensity.o \
	odtscene.o \
	odtauto.o \
	odtdata.o \
	odthistory.o \
	odtoutput.o \
	odtfft.o  \
	odtdrive.o \
	odtread.o

LOBJS = 
 
ALIB = 
 
PROG = nodt

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XW) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(GEMLIB) \
	$(CGEMLIB)

all : $(PROG)

CFLAGS = $(COPT) -I$(NAWIPS)/include

$(PROG): $(COBJS)
	$(LINK.f) $(NOFORMAIN) $(COBJS) $(LIBINC) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -lm -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
