#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = 

LOBJS = \
	$(ALIB)(gdigen.o) \
	$(ALIB)(gdiinp.o)

ALIB = $(OS_LIB)/gdinfo.a
 
PROG = gdinfo
PROG.f = gdinfo.f
 
LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)


all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

programs_nc:
	@echo "Not a graphic program, no _nc version needed"

programs_gf:
	@echo "Not a graphic program, no _gf version needed"

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

