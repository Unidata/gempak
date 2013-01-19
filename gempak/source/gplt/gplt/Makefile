#
#	Makefile for GEMPAK 6.0
#		Builds GPLT program
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

PROG.f = gplt.f

LIBINC = \
	$(GPLT) \
	$(GPLTDEV) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

LOBJS = 

PROG = $(PROG.f:.f=)

all : $(PROG)

$(PROG) : $(PROG.f) $(GPLT)
	$(LINK.f) $(PROG.f) $(LIBINC) -o $@

clean:
	$(RM) $(PROG) *.o 


install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	

