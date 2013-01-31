#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = sfcsplt
PROG.c = sfcsplt.c

LIBINC = \
	$(CGEMLIB) \
	$(APPL) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c) 
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(SYSLIBS) -o $@

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

