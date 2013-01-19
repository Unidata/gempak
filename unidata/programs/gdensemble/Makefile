#
#	Makefile for GEMPAK 5.6
#		Builds programs
#		Unidata 11/00 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = gdensemb.a

PROG.f = gdensemb.f

FOBJS = \
	$(ALIB)(gdenstat.o) \
	$(ALIB)(gdlinp.o) \
	$(ALIB)(gdensemb.o)

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

PROG = $(PROG.f:.f=)

all : $(ALIB) $(PROG)

$(PROG) : $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) -o $@

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
