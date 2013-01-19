#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = 
 
ALIB = 
 
PROG1 = rfop
PROG1.c = rfop.c

LIBINC = \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

all : $(PROG1)

$(PROG1): $(PROG1.c)
	$(COMPILE.c) $(PROG1.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(ALIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(CGEMLIB) $(APPL) -o $@

clean:
	$(RM) *.o $(PROG1)

install: $(PROG1)
	$(INSTALL) $(PROG1) $(OS_BIN)

