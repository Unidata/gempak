#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = gpclear
PROG.f = gpclear.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

.PRECIOUS: $(ALIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(LIBINC) -o $@

programs_gf:
	@echo "Nothing to make for _gf"

programs_nc:
	@echo "Nothing to make for _nc"
clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

