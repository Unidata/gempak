#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

PROG = ps
 
ALIB = $(OS_LIB)/ps.a
 
DEVLIBS = $(GN)
 
LOBJS = \
	$(ALIB)(hcirc.o) \
	$(ALIB)(hclear.o) \
	$(ALIB)(hclosp.o) \
	$(ALIB)(hdots.o) \
	$(ALIB)(hendd.o) \
	$(ALIB)(hfill.o) \
	$(ALIB)(hinit.o) \
	$(ALIB)(hinita.o) \
	$(ALIB)(hline.o) \
	$(ALIB)(hopen.o) \
	$(ALIB)(hqclrs.o) \
	$(ALIB)(hqcomp.o) \
	$(ALIB)(hqdatt.o) \
	$(ALIB)(hsatim.o) \
	$(ALIB)(hsbrgb.o) \
	$(ALIB)(hscint.o) \
	$(ALIB)(hscnam.o) \
	$(ALIB)(hscolb.o) \
	$(ALIB)(hscolr.o) \
	$(ALIB)(hscrgb.o) \
	$(ALIB)(hsdatt.o) \
	$(ALIB)(hsfill.o) \
	$(ALIB)(hsicmn.o) \
	$(ALIB)(hsltyp.o) \
	$(ALIB)(hslwid.o) \
	$(ALIB)(hstext.o) \
	$(ALIB)(htext.o) \
	$(ALIB)(htextc.o) \
	$(ALIB)(pcirc.o) \
	$(ALIB)(pclear.o) \
	$(ALIB)(pclosp.o) \
	$(ALIB)(pcvtclr.o) \
	$(ALIB)(pdots.o) \
	$(ALIB)(pendd.o) \
	$(ALIB)(pfill.o) \
	$(ALIB)(pfontn.o) \
	$(ALIB)(pinita.o) \
	$(ALIB)(pinitclr.o) \
	$(ALIB)(pline.o) \
	$(ALIB)(pqclrs.o) \
	$(ALIB)(pqcomp.o) \
	$(ALIB)(pqdatt.o) \
	$(ALIB)(psatim.o) \
	$(ALIB)(pscint.o) \
	$(ALIB)(pscnam.o) \
	$(ALIB)(pscolr.o) \
	$(ALIB)(pscrgb.o) \
	$(ALIB)(psdatt.o) \
	$(ALIB)(psfill.o) \
	$(ALIB)(psicmn.o) \
	$(ALIB)(psltyp.o) \
	$(ALIB)(pslwid.o) \
	$(ALIB)(psopen.o) \
	$(ALIB)(pstext.o) \
	$(ALIB)(ptext.o) \
	$(ALIB)(ptextc.o) \
	$(ALIB)(pupdclr.o)

LIBINC = \
	$(DEVICE) \
	$(ALIB) \
	$(DEVLIBS) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
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

