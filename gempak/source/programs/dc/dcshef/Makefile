#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = dcshef
 
OBJS = \
	dcshef.o

LIBINC = \
	$(BRIDGE) \
	$(OS_LIB)/libOHSHEF.a \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBNCEPBUFR) \
	$(OS_LIB)/libncepUT.a \
	$(LIBNCEPBUFR) \
	$(GPLT_NC) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(BRIDGE) \
        $(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(OBJS) 
	$(LINK.f) $(NOFORMAIN) -o $@ $(OBJS) $(LIBINC) $(CSYSLIB) -lm 

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

