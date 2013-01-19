#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = bnd2shp
PROG.c = bnd2shp.c
 

LIBINC = \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(OS_LIB)/libshp.a \
	$(GPLT) \
	$(CGEMLIB) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c) 
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(SYSLIBS) -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

