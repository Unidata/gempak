#
#	Makefile for GEMPAK 6.0
#		Builds NEX2IMG
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/nex2img.a

PROG.f = nex2img.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gdcanl.o) \
	$(ALIB)(gdcnav.o) \
	$(ALIB)(gdctbl.o)

COBJS = \
	$(ALIB)(radar_grid.o) \
	$(ALIB)(gdwgin.o) \
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
	$(RSLLIB) \
	$(LIBBZ2) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(LIBREADLINE)

PROG = $(PROG.f:.f=)
PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)

CFLAGS = $(COPT) -I$(GEMPAK)/source/driver/active/gif

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
	$(RM) $(PROG) *.o 


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) nex2img.pdf $(GEMPDF)
	$(AUXINSTALL) nex2img.prm $(GEMPARM)
	$(AUXINSTALL) nex2img.err $(GEMERR)
	$(AUXINSTALL) nex2img.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) nex2img.txt $(GEMPTXT)

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
