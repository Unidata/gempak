#
#	Makefile for GEMPAK 6.0
#		Builds GDRADR
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdradr.a

PROG.f = gdradr.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gdcanl.o) \
	$(ALIB)(gdcnav.o) \
	$(ALIB)(gdctbl.o)

COBJS = \
	$(ALIB)(radar_grid.o) \
	$(ALIB)(waittime.o)

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

all : $(ALIB) $(PROG)

$(PROG) : $(ALIB) $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o $(ALIB)


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) gdradr.pdf $(GEMPDF)
	$(AUXINSTALL) gdradr.prm $(GEMPARM)
	$(AUXINSTALL) gdradr.err $(GEMERR)
	$(AUXINSTALL) gdradr.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) gdradr.txt $(GEMPTXT)
	$(AUXINSTALL) raddur.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) radfrq.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) radtim.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) radmode.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) ndval.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) raddur.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) radfrq.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) radtim.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) radmode.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) ndval.hl2 $(GEMHLP)/hlx
