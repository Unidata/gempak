#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = dcigdr
 
OBJS = \
	dcigdr.o \
	igascii.o \
	igbfif.o \
	igbufr.o \
	igdcod.o \
	igifiv.o \
	igifpt.o \
	igifsp.o
 

LIBINC = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(OS_LIB)/libncepBUFR.a \
	$(OS_LIB)/libncepUT.a \
	$(OS_LIB)/libncepBUFR.a \
	$(GEMLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(OBJS) 
	$(LINK.f) $(NOFORMAIN) -o $@ $(OBJS) $(LIBINC) $(SYSLIBS) -L$(OS_LIB) -lm

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
