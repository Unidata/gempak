#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = vg
 
ALIB = $(OS_LIB)/vg.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(hcirc.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hegrp.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfrnt.o) \
	$(ALIB)(hhash.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hmark.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hrfill.o) \
	$(ALIB)(hrline.o) \
	$(ALIB)(hrspln.o) \
	$(ALIB)(hsclr2.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hsdash.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hsfrnt.o) \
	$(ALIB)(hsgrp.o) \
	$(ALIB)(hshash.o) \
	$(ALIB)(hsline.o) \
	$(ALIB)(hsmark.o) \
	$(ALIB)(hssmth.o) \
	$(ALIB)(hsspln.o) \
	$(ALIB)(hssymb.o) \
	$(ALIB)(hstext.o) \
	$(ALIB)(hsgtgn.o) \
	$(ALIB)(hswind.o) \
	$(ALIB)(hsymb.o) \
	$(ALIB)(htext.o) \
	$(ALIB)(htextc.o) \
	$(ALIB)(htxsy.o) \
	$(ALIB)(hwind.o) \
	$(ALIB)(vcirc.o) \
	$(ALIB)(vclosp.o) \
	$(ALIB)(vegrp.o) \
	$(ALIB)(vendd.o) \
	$(ALIB)(vfill.o) \
	$(ALIB)(vfrnt.o) \
	$(ALIB)(vhash.o) \
	$(ALIB)(vinita.o) \
	$(ALIB)(vline.o) \
	$(ALIB)(vmark.o) \
	$(ALIB)(vopen.o) \
	$(ALIB)(vsclr2.o) \
	$(ALIB)(vscolr.o) \
	$(ALIB)(vsdash.o) \
	$(ALIB)(vsdatt.o) \
	$(ALIB)(vsetgrps.o) \
	$(ALIB)(vsfill.o) \
	$(ALIB)(vsfrnt.o) \
	$(ALIB)(vsgrp.o) \
	$(ALIB)(vsgtgn.o) \
	$(ALIB)(vshash.o) \
	$(ALIB)(vsline.o) \
	$(ALIB)(vsmark.o) \
	$(ALIB)(vspln.o) \
	$(ALIB)(vssmth.o) \
	$(ALIB)(vsspln.o) \
	$(ALIB)(vssymb.o) \
	$(ALIB)(vstext.o) \
	$(ALIB)(vswind.o) \
	$(ALIB)(vsymb.o) \
	$(ALIB)(vtext.o) \
	$(ALIB)(vtxsy.o) \
	$(ALIB)(vwind.o)

LIBINC = \
	$(GEMLIB) \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(SYSLIB) \
	$(NEEDX11) \
	$(APPL) \
	$(NETCDF) \
	$(ZLIB) \
	$(LIBBZ2) \
	$(RSLLIB) \
	$(XMLLIB) \
	-lm


.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

all : device.f $(ALIB) $(PROG)

install:
	$(INSTALL) $(PROG) $(OS_BIN)

$(PROG): $(ALIB)
	$(LINK.f) device.f $(LIBINC) -o $@
	$(RM) device.f

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

