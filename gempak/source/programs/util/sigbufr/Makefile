#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/sigbufr.a

COBJS = 

FOBJS = \
	$(ALIB)(sigbcld.o) \
	$(ALIB)(sigbeof.o) \
	$(ALIB)(sigbeom.o) \
	$(ALIB)(sigberror.o) \
	$(ALIB)(sigbfronts.o) \
	$(ALIB)(sigbjets.o) \
	$(ALIB)(sigbmcld.o) \
	$(ALIB)(sigbradtn.o) \
	$(ALIB)(sigbstorm.o) \
	$(ALIB)(sigbtrop.o) \
	$(ALIB)(sigbturb.o) \
	$(ALIB)(sigbvolcano.o)

PROG = sigbufr
PROG.c = sigbufr.c
 
LIBINC = \
	$(ALIB) \
	$(GEMLIB) \
        $(CGEMLIB) \
	$(OS_LIB)/libmel_bufr.a \
	$(GEMLIB) \
        $(APPL) \
        $(SYSLIB) \
        $(NETCDF) \
        $(LIBJASPER) \
        $(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB)


CFLAGS = $(COPT) $(BUFRFLAGS)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(SYSLIBS) -o $@

$(ALIB): $(FOBJS) $(COBJS)
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

