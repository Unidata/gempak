#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = fax
 
ALIB = $(OS_LIB)/fax.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hdots.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(ras26bit.o) \
	$(ALIB)(rbrescirc.o) \
	$(ALIB)(rcircpts.o) \
	$(ALIB)(rclear.o) \
	$(ALIB)(rclosp.o) \
	$(ALIB)(rcvt2cdc.o) \
	$(ALIB)(rcvt6bt.o) \
	$(ALIB)(rdots.o) \
	$(ALIB)(rendd.o) \
	$(ALIB)(rfill.o) \
	$(ALIB)(rinita.o) \
	$(ALIB)(risched.o) \
	$(ALIB)(rline.o) \
	$(ALIB)(rmapid.o) \
	$(ALIB)(ropen.o) \
	$(ALIB)(rpadrec.o) \
	$(ALIB)(rrunle.o) \
	$(ALIB)(rscnfll.o) \
	$(ALIB)(rscolr.o) \
	$(ALIB)(rsdatt.o) \
	$(ALIB)(rsdump.o) \
	$(ALIB)(rsfill.o) \
	$(ALIB)(rsndsix.o) \
	$(ALIB)(rspan.o) \
	$(ALIB)(rwrpxl.o)

LIBINC = \
	$(GEMLIB) \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(SYSLIB) \
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

