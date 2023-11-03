#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(vg2frzl.o)

LOBJS = \
	$(ALIB)(gdninpfz.o)

ALIB = $(OS_LIB)/gdfrzl.a
 
PROG = gdfrzl
PROG.f = gdfrzl.f

LIBINC = \
	$(OS_LIB)/gdcntr.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(VG) $(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(CGEMLIB) \
	$(VG) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm \
	$(NETCDF) $(ZLIB) $(RSLLIB) $(LIBBZ2) -o $@

programs_gf:
	@echo "Not a graphic program, no _gf version needed"

programs_nc:
	@echo "Not a graphic program, no _nc version needed"

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

