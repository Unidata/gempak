#
#	Makefile for GEMPAK 5.2
#		Builds programs
#		Unidata 7/94 P.Bruehl
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdbiint.a

PROG = gdbiint

COBJS = 

FOBJS = \
	$(ALIB)(gdinterp.o) \
	$(ALIB)(gdlinp.o)

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

all : $(ALIB) $(PROG)

$(PROG) : $(LIBINC) $(PROG).f
	$(COMPILE.f) $(PROG).f
	$(LINK.f) $@.f $(ALIB) $(LIBINC) $(SYSLIBS) -o $@

programs_nc :
	@echo -n "Not a graphics program - no nc version needed"

programs_gf :
	@echo -n "Not a graphics program - no gf version needed"

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
	$(AUXINSTALL) $(PROG).pdf $(GEMPDF)
	$(AUXINSTALL) $(PROG).err $(GEMERR)
	$(AUXINSTALL) $(PROG).hlp $(GEMHLP)/hlp
	$(AUXINSTALL) $(PROG).prm $(GEMPARM)
	$(AUXINSTALL) $(PROG).txt $(GEMPTXT)
