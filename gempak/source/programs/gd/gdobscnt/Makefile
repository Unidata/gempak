#
#	Makefile for GEMPAK 5.6
#		Builds GDOBSCNT
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gdobscnt.a

PROG.f = gdobscnt.f

FOBJS = \
	$(ALIB)(gpinp.o) \
	$(ALIB)(sflfil.o)

COBJS = 

LIBINC = \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB) \
	$(GEMLIB)

PROG = $(PROG.f:.f=)

all : $(ALIB) $(PROG)

$(PROG) : $(LIBINC) $(PROG.f) 
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@

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
	$(AUXINSTALL) gdobscnt.err $(GEMERR)
	$(AUXINSTALL) gdobscnt.pdf $(GEMPDF)
	$(AUXINSTALL) gdobscnt.prm $(GEMPARM)
	$(AUXINSTALL) gdobscnt.txt $(GEMPTXT)
	$(AUXINSTALL) gdobscnt.hlp $(GEMHLP)/hlp
	$(AUXINSTALL) radius.hl1 $(GEMHLP)/hlx
	$(AUXINSTALL) radius.hl2 $(GEMHLP)/hlx
