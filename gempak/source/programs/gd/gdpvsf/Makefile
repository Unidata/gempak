#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = 


LOBJS = \
	$(ALIB)(gdpvcl.o) \
	$(ALIB)(gdpvdt.o) \
	$(ALIB)(gdpvin.o) \
	$(ALIB)(gdpvlv.o)

ALIB = $(GEMOLB)/gdpvsf.a
 
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
	$(ZLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@


programs_nc:
	@echo "Not a graphic program, no_nc version needed"

programs_gf:
	@echo "Not a graphic program, no_gf version needed"

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
	$(INSTALL) $(PROG) $(GEMEXE)
