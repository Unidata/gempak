#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

LOBJS = 

COBJS = \
	$(ALIB)(gdg2in.o) \
	$(ALIB)(gdgetgrid.o) \
	$(ALIB)(gdmakeg2.o) \
	$(ALIB)(gdmakepdt.o) \
	$(ALIB)(gdmakwmo.o) \
	$(ALIB)(gdparseparm.o) \
	$(ALIB)(gdrdln.o) \
	$(ALIB)(gdsetnav.o) \
	$(ALIB)(gdsetsect1.o) \
	$(ALIB)(gdsetsect4.o) \
	$(ALIB)(gdsetsect5.o)

ALIB = $(OS_LIB)/gdgrib2.a
 
PROG = gdgrib2
PROG.c = $(PROG).c

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $(PROG).o $(ALIB) $(LIBINC) \
	$(LIBJASPER) $(SYSLIBS) -lm $(PNGLIB) $(ZLIB) -o $@
	$(RM) *.o

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

