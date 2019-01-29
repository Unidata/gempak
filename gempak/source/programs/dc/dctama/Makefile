#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
# $(BINDIR)/dctama: -ldctama -lbridge -lsnlib -lgemlib -lprmcnvlib \
#                   -lncepBUFR -lncepUT -lncepBUFR \
#                                     -ltextlib -lgemlib $(PYLIB) -lxslt -lxml2 -liconv -lz -lm
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
        $(CGEMLIB) \
        $(OS_LIB)/libncepBUFR.a \
        $(OS_LIB)/libncepUT.a \
        $(OS_LIB)/libncepBUFR.a \
	$(XMLLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB)
 

all : $(PROG)

$(PROG): $(LIBINC) $(OBJS) 
	$(LINK.f) $(NOFORMAIN) -o $@ $(OBJS) $(LIBINC) $(CSYSLIB) $(SYSLIB)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

