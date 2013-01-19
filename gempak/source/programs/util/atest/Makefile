#
#	Makefile for GEMPAK 6.0
#		Builds tesprograms
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROGS = \
	atest
 
OBJS = 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB)

all : $(PROGS)

$(PROGS): $(LIBINC) $(OBJS) 
	$(LINK.f) -o $@ $@.f $(OBJS) $(LIBINC) $(CSYSLIB)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROGS)

install: $(PROGS)
	$(INSTALL) atest $(OS_BIN)

