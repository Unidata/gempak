#
#	Makefile for GEMPAK 7.0.4
#		Builds NEXRCOMP
#		Unidata M.James 03/14
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nexrcomp.a

PROG.f = nexrcomp.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gdcanl.o) \
	$(ALIB)(gdcnav.o) \
	$(ALIB)(gdctbl.o)

COBJS = \
	$(ALIB)(radar_grid.o) \
	$(ALIB)(waittime.o) \
	$(ALIB)(writepng.o)

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GIF) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

PROG = $(PROG.f:.f=)
PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)

CFLAGS = $(COPT) -I$(PNGINC)

all : $(ALIB) $(PROG)

$(PROG) : $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o $(OS_LIB)/nexrcomp.a


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) nexrcomp.pdf $(GEMPDF)
	$(AUXINSTALL) nexrcomp.prm $(GEMPARM)
	$(AUXINSTALL) nexrcomp.err $(GEMERR)
	$(AUXINSTALL) nexrcomp.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) nexrcomp.txt $(GEMPTXT)

#
#	These should come from gdradr
#	$(AUXINSTALL) raddur.hl1 $(GEMHLP)/hlx
#	$(AUXINSTALL) radfrq.hl1 $(GEMHLP)/hlx
#	$(AUXINSTALL) radtim.hl1 $(GEMHLP)/hlx
#	$(AUXINSTALL) radmode.hl1 $(GEMHLP)/hlx
#	$(AUXINSTALL) raddur.hl2 $(GEMHLP)/hlx
#	$(AUXINSTALL) radfrq.hl2 $(GEMHLP)/hlx
#	$(AUXINSTALL) radtim.hl2 $(GEMHLP)/hlx
#	$(AUXINSTALL) radmode.hl2 $(GEMHLP)/hlx
