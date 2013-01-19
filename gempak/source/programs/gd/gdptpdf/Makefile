#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)


LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(GEMLIB) \
	$(SYSLIB) \
	$(NETCDF) \
	$(ZLIB)

COBJS =

LOBJS = \
	$(ALIB)(gdptdat.o) \
	$(ALIB)(gdptdsp.o) \
	$(ALIB)(gdptdtm.o) \
	$(ALIB)(gdptgrf.o) \
	$(ALIB)(gdptinp.o) \
	$(ALIB)(gdptout.o) \
	$(ALIB)(gdptplt.o) \
	$(ALIB)(gdptxax.o) \
	$(ALIB)(gdptyax.o) \
	$(ALIB)(gdptymx.o)

ALIB = $(OS_LIB)/gdptpdf.a
 
PROG = gdptpdf
PROG.f = gdptpdf.f
 

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
	$(CGEMLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@


programs_gf:
	@echo "Not a graphic program, no _gf version needed"

programs_nc:
	@echo "Not a graphic program, no _nc version needed"

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

