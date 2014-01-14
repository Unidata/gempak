#
#	Makefile for GEMPAK 6.0
#		Builds NEX2GINI
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nex2gini.a

PROG.f = nex2gini.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gdcanl.o) \
	$(ALIB)(gdcnav.o) \
	$(ALIB)(gdctbl.o)

COBJS = \
	$(ALIB)(radar_grid.o) \
	$(ALIB)(gdlgin.o) \
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
	$(RM) $(PROG) *.o 


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) nex2gini.pdf $(GEMPDF)
	$(AUXINSTALL) nex2gini.prm $(GEMPARM)
	$(AUXINSTALL) nex2gini.err $(GEMERR)
	$(AUXINSTALL) nex2gini.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) nex2gini.txt $(GEMPTXT)
	$(AUXINSTALL) compress.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) compress.hl2 $(GEMHLP)/hlx

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
