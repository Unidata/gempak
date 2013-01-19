#
#	Makefile for GEMPAK 6.0
#		Builds GPRCM
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gprcm.a

PROG.f = gprcm.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(gpmopt.o) \
	$(ALIB)(gdctbl.o) \
	$(ALIB)(text_output.o)

COBJS = \
	$(ALIB)(box_fill.o) \
	$(ALIB)(bulletin_utils.o) \
	$(ALIB)(filter.o) \
	$(ALIB)(mdr_utils.o) \
	$(ALIB)(radar_info.o) \
	$(ALIB)(radar_stats.o) \
	$(ALIB)(rcm.o) \
	$(ALIB)(read_nexaa.o) \
	$(ALIB)(read_nexcc.o) 

LIBINC = \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(NETCDF) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_NC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(NC) \
	$(GN) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
        $(CGEMLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

LIBINC_GF = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GF) \
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
	$(AUXINSTALL) gprcm.pdf $(GEMPDF)
	$(AUXINSTALL) gprcm.prm $(GEMPARM)
	$(AUXINSTALL) gprcm.err $(GEMERR)
	$(AUXINSTALL) gprcm.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) gprcm.txt $(GEMPTXT)
	$(CP) *.hl1 *.hl2 $(GEMHLP)/hlx
