#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROGS = \
	naltxt
 
OBJS = \
	label.o  \
	list.o  \
	text.o  \
	viewfile.o

PROGO = \
	naltxt.o
 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NXMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
        $(SYSLIB) \
        $(NETCDF) \
        $(LIBJASPER) \
        $(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB) \
	$(CGEMLIB)


CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(PROGS)

$(PROGS): $(LIBINC) $(OBJS) $(PROGO)
	$(LINK.f) $(NOFORMAIN) -o $@ $@.o $(OBJS) $(LIBINC) \
	$(SYSLIBS) $(GUILIBS) $(CSYSLIB)

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"


clean:
	$(RM) *.o $(PROGS)

install: $(PROGS)
	$(INSTALL) naltxt $(OS_BIN)
