#
#	Makefile for GEMPAK 6.0
#		Builds GDRADRC
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdradrc.a

PROG.f = gdradrc.f

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
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(LIBREADLINE)

PROG = $(PROG.f:.f=)
PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)

all : $(ALIB) $(PROG)

$(PROG) : $(ALIB) $(LIBINC) $(PROG.f) 
	$(COMPILE.f) $(PROG.f)
	$(LINK.f) $(PROG).o $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

clean:
	$(RM) $(PROG) *.o $(ALIB)


install: $(PROG) 
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) gdradrc.pdf $(GEMPDF)
	$(AUXINSTALL) gdradrc.prm $(GEMPARM)
	$(AUXINSTALL) gdradrc.err $(GEMERR)
	$(AUXINSTALL) gdradrc.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) gdradrc.txt $(GEMPTXT)
