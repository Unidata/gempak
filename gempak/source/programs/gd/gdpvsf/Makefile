#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdpvsf.a
 
LOBJS = \
	$(ALIB)(gdpvcl.o) \
	$(ALIB)(gdpvdt.o) \
	$(ALIB)(gdpvin.o) \
	$(ALIB)(gdpvlv.o) 

PROG = gdpvsf
PROG.f = gdpvsf.f


 
LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(XMLLIB) \
	$(CGEMLIB) \
	$(PNGLIB) \
	$(ZLIB) \
	$(LIBREADLINE)
 

.PRECIOUS: $(ALIB)

all: $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

programs_nc:
	@echo "Not a graphic program, no_nc version needed"

programs_gf:
	@echo "Not a graphic program, no_gf version needed"

$(ALIB): $(LOBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) $(ALIB)
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
