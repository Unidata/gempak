#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(sfkdat.o) \
	$(ALIB)(sfkhdr.o) \
	$(ALIB)(sfkinp.o) \
	$(ALIB)(sfkosf.o)
 
ALIB = $(OS_LIB)/sfchck.a
 
PROG = sfchck
PROG.f = sfchck.f
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(XMLLIB) \
        $(ZLIB)

.PRECIOUS: $(ALIB)

FFLAGS = $(FOPT) $(NEED_U77)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@
	$(RM) *.o

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

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

