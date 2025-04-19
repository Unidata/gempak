#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = $(ALIB)(gdedat.o) 

LOBJS = \
	$(ALIB)(gdedatf.o) \
	$(ALIB)(gdedsp.o) \
	$(ALIB)(gdegdt.o) \
	$(ALIB)(gdeghd.o) \
	$(ALIB)(gdeidn.o) \
	$(ALIB)(gdeinp.o) \
	$(ALIB)(gdescl.o) \
	$(ALIB)(gdesiz.o)

ALIB = $(OS_LIB)/gdedit.a
 
PROG = gdedit
PROG.f = gdedit.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(GEMLIB)\
	$(LIBJASPER) \
	$(PNGLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(CSYSLIB) -o $@

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

