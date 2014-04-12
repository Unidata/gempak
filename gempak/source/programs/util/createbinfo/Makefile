#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#CFLAGS += -I$(GEMPAK)/source/cgemlib/clo
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = createbinfo
PROG.c = createbinfo.c
 
CFLAGS += -I$(GEMPAK)/source/cgemlib/clo

LIBINC = \
	$(CGEMLIB)

all : $(PROG)

$(PROG): $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c) 
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) $(SYSLIBS) -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

