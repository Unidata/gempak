#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

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
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) $(XMLLIB) $(ZLIB) \
	$(LIBREADLINE)

.PRECIOUS: $(ALIB)

FFLAGS = $(FOPT) $(NEED_U77)

all : $(ALIB) $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@
	$(RM) *.o

#$FC $FFLAGS $LDFLAGS -o $OS_BIN/sfchck sfchck.f \
#	$OS_LIB/sfchck.a \
#	$GEMLIB $APPL $SYSLIB $CGEMLIB
#$RM *.o

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

$(ALIB): $(LOBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) $(LDFLAGS) -c $<

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

