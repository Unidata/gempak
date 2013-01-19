#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROGS = \
	nalarm
 
OBJS = 

PROGO = \
	nalarm.o
 

LIBINC = \
	$(GEMLIB) \
	$(NXMLIB) \
	$(GEMLIB) \
	$(CGEMLIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(PROGS)

$(PROGS): $(LIBINC) $(OBJS) $(PROGO)
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $(OBJS) $(LIBINC) \
	$(GUILIBS) $(CSYSLIB)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROGS)

install: $(PROGS)
	$(INSTALL) nalarm $(OS_BIN)
