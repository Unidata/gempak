#
#	Makefile for GEMPAK 5.6
#
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/sndiag.a

PROG.f = sndiag.f

LIBINC = \
	$(GEMLIB) \
	$(APPL) \
	$(SYSLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(XMLLIB) \
        $(ZLIB) \
        $(GEMLIB) \
        $(CGEMLIB) \
	$(LIBREADLINE)

FOBJS = \
	$(ALIB)(sndarr.o) \
	$(ALIB)(sndbvs.o) \
	$(ALIB)(sndcmp.o) \
	$(ALIB)(snddta.o) \
	$(ALIB)(snddtm.o) \
	$(ALIB)(sndeml.o) \
	$(ALIB)(sndesp.o) \
	$(ALIB)(sndest.o) \
	$(ALIB)(sndfil.o) \
	$(ALIB)(sndflt.o) \
	$(ALIB)(sndflx.o) \
	$(ALIB)(sndfsp.o) \
	$(ALIB)(sndhlc.o) \
	$(ALIB)(sndhtr.o) \
	$(ALIB)(sndiag.o) \
	$(ALIB)(sndinp.o) \
	$(ALIB)(sndint.o) \
	$(ALIB)(sndlev.o) \
	$(ALIB)(sndlid.o) \
	$(ALIB)(sndlsi.o) \
	$(ALIB)(sndlyr.o) \
	$(ALIB)(sndmix.o) \
	$(ALIB)(sndopn.o) \
	$(ALIB)(sndprm.o) \
	$(ALIB)(sndrch.o) \
	$(ALIB)(sndrhb.o) \
	$(ALIB)(sndrhf.o) \
	$(ALIB)(sndrrc.o) \
	$(ALIB)(sndscr.o) \
	$(ALIB)(sndshd.o) \
	$(ALIB)(sndshm.o) \
	$(ALIB)(sndshr.o) \
	$(ALIB)(sndspl.o) \
	$(ALIB)(sndthw.o) \
	$(ALIB)(sndtrp.o) \
	$(ALIB)(sndtwb.o) \
	$(ALIB)(sndupd.o) \
	$(ALIB)(sndwnd.o) \
	$(ALIB)(sndwob.o)

PROG = $(PROG.f:.f=)

all : $(ALIB) $(PROG) 

$(PROG) : $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(CSYSLIB) -o $@

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

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
	$(AUXINSTALL) *.hl2 $(GEMHLP)/hlx
	$(AUXINSTALL) *.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) *.pdf $(GEMPDF)
	$(AUXINSTALL) *.err $(GEMERR)

clean:
	$(RM) $(PROG) $(ALIB) *.o

