#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =	$(ALIB)(imrfax.o)

LOBJS = \
	$(ALIB)(gpfinp.o) \
	$(ALIB)(gpfopt.o)

ALIB = $(OS_LIB)/gpfax.a
 
PROG = gpfax
PROG.f = gpfax.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)\
	$(GN) \
        $(XMLLIB) \
        $(ZLIB)



.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

programs_ps :
	@echo "Nothing to make for _ps"

programs_gf :
	@echo "Nothing to make for _gf"

programs_nc :
	@echo "Nothing to make for _nc"

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

$(ALIB): $(LOBJS) $(COBJS)
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

