#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = rbk
 
ALIB = $(OS_LIB)/rbk.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(aclear.o) \
	$(ALIB)(aclosp.o) \
	$(ALIB)(aegrp.o) \
	$(ALIB)(aendd.o) \
	$(ALIB)(afill.o) \
	$(ALIB)(ainita.o) \
	$(ALIB)(aline.o) \
	$(ALIB)(aopen.o) \
	$(ALIB)(aqdatt.o) \
	$(ALIB)(ascolr.o) \
	$(ALIB)(asdatt.o) \
	$(ALIB)(asgrp.o) \
	$(ALIB)(astext.o) \
	$(ALIB)(asymb.o) \
	$(ALIB)(atext.o) \
	$(ALIB)(awrbuf.o) \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hegrp.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hqdatt.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsgrp.o) \
	$(ALIB)(hstext.o) \
	$(ALIB)(hsymb.o) \
	$(ALIB)(htext.o) \
	$(ALIB)(htextc.o)

LIBINC = \
	$(GEMLIB) \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NEEDX11) \
	-lm

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : $(ALIB) $(PROG)

install:
	$(INSTALL) $(PROG) $(OS_BIN)

$(PROG): $(ALIB)
	$(LINK.f) device.f $(LIBINC) -o $@
	$(RM) device.o

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

