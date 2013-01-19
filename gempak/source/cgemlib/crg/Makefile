#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

GDIR = crg

COBJS = \
	$(ALIB)(crgbuild.o) \
	$(ALIB)(crgclear.o) \
	$(ALIB)(crgclearlayer.o) \
	$(ALIB)(crgclroffst.o) \
	$(ALIB)(crgdeselect.o) \
	$(ALIB)(crggassocrec.o) \
	$(ALIB)(crggbnd.o) \
	$(ALIB)(crgget.o) \
	$(ALIB)(crggetfilter.o) \
	$(ALIB)(crggetinx.o) \
	$(ALIB)(crggetlayer.o) \
	$(ALIB)(crggetsecrange.o) \
	$(ALIB)(crggettxtbox.o) \
	$(ALIB)(crgggbnd.o) \
	$(ALIB)(crggginx.o) \
	$(ALIB)(crgggnel.o) \
	$(ALIB)(crgggnhl.o) \
	$(ALIB)(crgggnxt.o) \
	$(ALIB)(crgggrp.o) \
	$(ALIB)(crggoffset.o) \
	$(ALIB)(crggoffsets.o) \
	$(ALIB)(crggrfrsh.o) \
	$(ALIB)(crggsel.o) \
	$(ALIB)(crggtyp.o) \
	$(ALIB)(crginit.o) \
	$(ALIB)(crgisauxrec.o) \
	$(ALIB)(crglkattr.o) \
	$(ALIB)(crgmkrange.o) \
	$(ALIB)(crgmvallayer.o) \
	$(ALIB)(crgnewinx.o) \
	$(ALIB)(crgrebuild.o) \
	$(ALIB)(crgrngpt.o) \
	$(ALIB)(crgsarec.o) \
	$(ALIB)(crgsave.o) \
	$(ALIB)(crgset.o) \
	$(ALIB)(crgsetauxrec.o) \
	$(ALIB)(crgsetcir.o) \
	$(ALIB)(crgsetfilter.o) \
	$(ALIB)(crgsetlayer.o) \
	$(ALIB)(crgsetlist.o) \
	$(ALIB)(crgsetltln.o) \
	$(ALIB)(crgsetsigmet.o) \
	$(ALIB)(crgsetsym.o) \
	$(ALIB)(crgsettxt.o) \
	$(ALIB)(crgsetvac.o) \
	$(ALIB)(crgsetvol.o) \
	$(ALIB)(crgsetwnd.o) \
	$(ALIB)(crgsetwbx.o) \
	$(ALIB)(crgsgrp.o) \
	$(ALIB)(crgsoffset.o) \
	$(ALIB)(crgssel.o) \
	$(ALIB)(crgstyp.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

