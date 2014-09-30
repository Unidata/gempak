#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS = 

LOBJS = \
	$(ALIB)(sfldat.o) \
	$(ALIB)(sfldta.o) \
	$(ALIB)(sflfil.o) \
	$(ALIB)(sflhdr.o) \
	$(ALIB)(sflinp.o) \
	$(ALIB)(sfllis.o) \
	$(ALIB)(sflprc.o) \
	$(ALIB)(sflprm.o) \
	$(ALIB)(sflprt.o)
 
ALIB = $(OS_LIB)/sflist.a
 
PROG = sflist
PROG.f = sflist.f
 

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(TEXTLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XSLTLIB) \
	$(XMLLIB) \
        $(ZLIB) \
	$(GEMLIB)


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

