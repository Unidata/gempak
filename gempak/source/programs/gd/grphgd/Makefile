#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(ggoadl.o) \
	$(ALIB)(ggsort.o)

LOBJS = \
	$(ALIB)(gbound.o) \
	$(ALIB)(gciijj.o) \
	$(ALIB)(gdline.o) \
	$(ALIB)(gdlwei.o) \
	$(ALIB)(ggapsm.o) \
	$(ALIB)(ggbndconds.o) \
	$(ALIB)(ggcrinfo.o) \
	$(ALIB)(ggdriv.o) \
	$(ALIB)(ggedge.o) \
	$(ALIB)(gggint.o) \
	$(ALIB)(gglims.o) \
	$(ALIB)(ggmnmx.o) \
	$(ALIB)(ggoacr.o) \
	$(ALIB)(ggrdinfo.o) \
	$(ALIB)(gindex.o) \
	$(ALIB)(gindx1.o) \
	$(ALIB)(glastv.o) \
	$(ALIB)(glimit.o) \
	$(ALIB)(glvalu.o) \
	$(ALIB)(gnextv.o) \
	$(ALIB)(gqintr.o) \
	$(ALIB)(gradls.o) \
	$(ALIB)(grginp.o) \
	$(ALIB)(gsmoot.o) \
	$(ALIB)(gsortd.o) \
	$(ALIB)(gweigs.o) \
	$(ALIB)(lnsgi1.o) \
	$(ALIB)(lnsgin.o)

ALIB = $(OS_LIB)/grphgd.a
 
PROG = grphgd
PROG.c = $(PROG).c

LIBINC = \
	$(OS_LIB)/gdcfil.a \
	$(OS_LIB)/oabsfc.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GN) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.c)
	$(COMPILE.c) $(PROG.c)
	$(LINK.f) $(NOFORMAIN) $@.o $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

programs_gf:
	@echo "Not a graphic program, no _gf version needed"

programs_nc:
	@echo "Not a graphic program, no _nc version needed"

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

