#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = utf
 
ALIB = $(OS_LIB)/utf.a
 
DEVLIBS = $(GN)
 
LOBJS = \
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
	$(ALIB)(htextc.o) \
	$(ALIB)(uclear.o) \
	$(ALIB)(uclosp.o) \
	$(ALIB)(uegrp.o) \
	$(ALIB)(uendd.o) \
	$(ALIB)(ufill.o) \
	$(ALIB)(uinita.o) \
	$(ALIB)(uline.o) \
	$(ALIB)(uopen.o) \
	$(ALIB)(uqdatt.o) \
	$(ALIB)(uscolr.o) \
	$(ALIB)(usdatt.o) \
	$(ALIB)(usgrp.o) \
	$(ALIB)(ustext.o) \
	$(ALIB)(uswap.o) \
	$(ALIB)(usymb.o) \
	$(ALIB)(utext.o) \
	$(ALIB)(uwrbuf.o)

LIBINC = \
	$(GEMLIB) \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(SYSLIB) \
	$(NEEDX11) \
	-lm

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : device.f $(ALIB) $(PROG)

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


device.f :
	$(LN) $(GEMPAK)/source/device/main/device.f .

clean:
	$(RM) *.o device.f $(PROG)

