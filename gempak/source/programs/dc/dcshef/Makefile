#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#$(BINDIR)/dcshef: $(OS_LIB)/ginitp_alt.o $(OS_LIB)/gendp_alt.o \
#           -lbridge -lprmcnvlib -lsflib -lsnlib -lOHSHEF -lbridge \
#           -lcgemlib -lncepBUFR -lncepUT -lncepBUFR -lgplt \
#           -ldevice -lgn -lbridge -lcgemlib -lgemlib \
#           -ltextlib -lgemlib $(PYLIB) -lxslt -lxml2 -liconv -lz -lm

include $(CONFIGDIR)/$(MAKEINC)

PROG = dcshef
 
OBJS = \
	dcshef.o

LIBINC = \
	$(BRIDGE) \
	$(GEMLIB) \
	$(SFLIB) \
	$(SNLIB) \
	$(OS_LIB)/libOHSHEF.a \
	$(BRIDGE) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBNCEPBUFR) \
	$(OS_LIB)/libncepUT.a \
	$(LIBNCEPBUFR) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(BRIDGE) \
	$(CGEMLIB) \
	$(GEMLIB) \
        $(TEXTLIB) \
	$(GEMLIB) \
	$(PYLIB) \
        $(XMLLIB) \
        $(ZLIB) \

all : $(PROG)

$(PROG): $(LIBINC) $(OBJS) 
	$(LINK.f) $(NOFORMAIN) -o $@ $(OBJS) $(LIBINC) $(SYSLIB) $(CSYSLIB) -lm 

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

