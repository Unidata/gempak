#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = dcxmos
 
OBJS = \
	dcgfsx.o  \
	dcxmos.o
 

LIBINC = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
        $(XMLLIB) \
        $(ZLIB)


all : $(PROG)

$(PROG): $(LIBINC) $(OBJS) 
	$(LINK.f) $(NOFORMAIN) -o $@ $(OBJS) $(LIBINC) $(CSYSLIB) $(SYSLIBS)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

