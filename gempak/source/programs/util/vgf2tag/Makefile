#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = 
 
ALIB = 
 
PROG = tag2vgf vgf2tag

LIBINC = \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(APPL) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(SYSLIB)

all : $(PROG)

tag2vgf: tag2vgf.c
	$(COMPILE.c) $?
	$(LINK.f) $(NOFORMAIN) $@.o $(CGEMLIB) $(GEMLIB) $(CGEMLIB) $(GEMLIB) -lm -o $@

vgf2tag: vgf2tag.c
	$(COMPILE.c) $?
	$(LINK.f) $(NOFORMAIN) $@.o $(LIBINC) -o $@

clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) tag2vgf $(OS_BIN)
	$(INSTALL) vgf2tag $(OS_BIN)
