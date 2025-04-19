#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = 

LOBJS = \
	$(ALIB)(gdwdsp.o) \
	$(ALIB)(gdwinp.o)

ALIB = $(OS_LIB)/gdwind.a
 
PROG = gdwind
PROG.f = gdwind.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
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

LIBINC_NC = \
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
	$(XMLLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_GF = \
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

PROG_NC = $(PROG)_nc
PROG_GF = $(PROG)_gf

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
		$(X11LIBDIR) $(X11LIB) $(SYSLIBS) $(SYSLIBS) -lm -o $(OS_BIN)/$@

programs_gf: $(PROG_GF)

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

