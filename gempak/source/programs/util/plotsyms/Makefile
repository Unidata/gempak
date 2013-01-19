#
#	Makefile for GEMPAK 6.0
#		Builds tesprograms
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROGS = \
	plotsyms
 
OBJS = 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(ZLIB)

all : $(PROGS)

$(PROGS): $(LIBINC) $(OBJS) 
	$(LINK.f) -o $@ $@.f $(OBJS) $(LIBINC)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROGS)

install: $(PROGS)
	$(INSTALL) plotsyms $(OS_BIN)

