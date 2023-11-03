#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(gdldsp.o) \
	$(ALIB)(gdldta.o) \
	$(ALIB)(gdlinp.o)

LOBJS = 

ALIB = $(OS_LIB)/gdlist.a
 
PROG = gdlist
PROG.c = gdlist.c
 

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
	$(RSLLIB) \
	$(LIBBZ2) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $@.o $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

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

