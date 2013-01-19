#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = dctama
 
OBJS = \
	dctama.o \
	tabfif.o \
	tabufr.o \
	tadcod.o \
	taifiv.o \
	taifpt.o \
	taifsp.o \
	tatmst.o

 

LIBINC = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) 

all : $(PROG)

$(PROG): $(LIBINC) $(OBJS) 
	$(LINK.f) $(NOFORMAIN) -o $@ $(OBJS) $(LIBINC) $(CSYSLIB)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

