#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(dinita2.o) \
	$(ALIB)(ggmap2.o) \
	$(ALIB)(ggsplt2.o) \
	$(ALIB)(imdrop2.o) \
	$(ALIB)(imlutf2.o) \
	$(ALIB)(imrtbl2.o)

FOBJS = \
	imnidh2.o

#	imcbar2.o

ALIB = $(OS_LIB)/radmap.a
 
PROG = radmap
PROG.f = radmap.f
PROGSW = radmap_sw
PROGSW.f = radmap_sw.f
 

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XW) \
	$(GF) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINCSW = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(OS_LIB)/gif.a \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

.PRECIOUS: $(ALIB)

FFLAGS = $(FOPT) $(NEED_U77)

all : $(ALIB) $(PROG) $(PROGSW)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f) $(FOBJS)
	$(LINK.f) $(PROG.f) $(FOBJS) $(ALIB) $(LIBINC) $(X11LIBDIR) $(X11LIB) $(SYSLIBS) -lm -o $@

$(PROGSW): $(ALIB) $(LIBINCSW) $(PROGSW.f) $(FOBJS)
	$(LINK.f) $(PROGSW.f) $(FOBJS) $(ALIB) $(LIBINCSW) $(SYSLIBS) -lm -o $@

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG) $(PROGSW) $(ALIB)

install: $(PROG) $(PROGSW)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(INSTALL) $(PROGSW) $(OS_BIN)
