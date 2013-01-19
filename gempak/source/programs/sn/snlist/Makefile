#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(snldat.o) \
	$(ALIB)(snldta.o) \
	$(ALIB)(snlfil.o) \
	$(ALIB)(snlinp.o) \
	$(ALIB)(snllev.o) \
	$(ALIB)(snllvd.o) \
	$(ALIB)(snlpdt.o) \
	$(ALIB)(snlprm.o) \
	$(ALIB)(snlprt.o) \
	$(ALIB)(snlwlp.o) \
	$(ALIB)(snlwmn.o) \
	$(ALIB)(snlwpm.o) \
	$(ALIB)(snlwsg.o) \
	$(ALIB)(snlwsp.o) \
	$(ALIB)(snlwst.o) \
	$(ALIB)(snlwtm.o)

ALIB = $(OS_LIB)/snlist.a
 
PROG = snlist
PROG.f = snlist.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

programs_gf:
	@echo "Not a graphic program, no _gf program needed"

programs_nc:
	@echo "Not a graphic program, no _nc program needed"

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

