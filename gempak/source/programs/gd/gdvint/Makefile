#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

LIBINC =

COBJS = \
	$(ALIB)(vidrivc.o)

LOBJS = \
	$(ALIB)(vcbldz.o) \
	$(ALIB)(vcetac.o) \
	$(ALIB)(vclnin.o) \
	$(ALIB)(vcmsht.o) \
	$(ALIB)(vcnbts.o) \
	$(ALIB)(vcpfet.o) \
	$(ALIB)(vcpfsg.o) \
	$(ALIB)(vcpfth.o) \
	$(ALIB)(vcpont.o) \
	$(ALIB)(vcponz.o) \
	$(ALIB)(vcquad.o) \
	$(ALIB)(vcsgma.o) \
	$(ALIB)(vcthta.o) \
	$(ALIB)(vcvapr.o) \
	$(ALIB)(vgexit.o) \
	$(ALIB)(vgierr.o) \
	$(ALIB)(vgigen.o) \
	$(ALIB)(vglvpr.o) \
	$(ALIB)(vgopen.o) \
	$(ALIB)(vgread.o) \
	$(ALIB)(vgstrt.o) \
	$(ALIB)(vgusin.o) \
	$(ALIB)(vgwrit.o) \
	$(ALIB)(vgwsfc.o) \
	$(ALIB)(vibddn.o) \
	$(ALIB)(vicovc.o) \
	$(ALIB)(vidriv.o) \
	$(ALIB)(vigetp.o) \
	$(ALIB)(vigetq.o) \
	$(ALIB)(vigetz.o) \
	$(ALIB)(vigtsf.o) \
	$(ALIB)(viinit.o) \
	$(ALIB)(vilnpo.o) \
	$(ALIB)(vimxmn.o) \
	$(ALIB)(vipool.o) \
	$(ALIB)(virdin.o) \
	$(ALIB)(virdtk.o) \
	$(ALIB)(vivcrd.o) \
	$(ALIB)(vivint.o)

ALIB = $(OS_LIB)/gdvint.a
 
PROG = gdvint
PROG.f = gdvint.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
        $(XMLLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(ZLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) $(CSYSLIB) -o $@

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

