#
#	Makefile for GEMPAK 6.0
#		Builds GPTRAJ
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gptraj.a

PROG.f = gptraj.f

FOBJS = \
	$(ALIB)(ggtraj.o) \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gpmopt.o) \
	$(ALIB)(gdctbl.o)

COBJS = 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GF) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(LIBREADLINE)

PROG = $(PROG.f:.f=)
PROG_NC = $(PROG.f:.f=_nc)
PROG_GF = $(PROG.f:.f=_gf)

all : $(ALIB) $(PROG)

$(PROG) : $(ALIB) $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

programs_nc : $(PROG_NC)

programs_gf : $(PROG_GF)

$(PROG_NC) : $(ALIB) $(LIBINC_NC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_NC) $(SYSLIBS) -lm -o $(OS_BIN)/$@

$(PROG_GF) : $(ALIB) $(LIBINC_GF) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC_GF) \
	$(X11LIBDIR) $(XWLIBS) $(SYSLIBS) -lX11 -lm -o $(OS_BIN)/$@

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

docs:
	hlpbld . gptraj
	pdfbld . gptraj

docinstall:
	$(AUXINSTALL) gptraj.err $(GEMERR)
	$(AUXINSTALL) gptraj.txt $(GEMPTXT)
	$(AUXINSTALL) gptraj.prm $(GEMPARM)
	$(AUXINSTALL) gptraj.pdf $(GEMPDF)
	$(AUXINSTALL) gptraj.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) rtraj.hl[12] $(GEMHLP)/hlx
	$(AUXINSTALL) tstep.hl[12] $(GEMHLP)/hlx
