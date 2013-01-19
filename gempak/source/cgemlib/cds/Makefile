#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(cdsash.o) \
	$(ALIB)(cdsatdeflt.o) \
	$(ALIB)(cdsccf.o) \
	$(ALIB)(cdsclass.o) \
	$(ALIB)(cdscirc.o) \
	$(ALIB)(cdsdspelm.o) \
	$(ALIB)(cdsfrnt.o) \
	$(ALIB)(cdsgetinx.o) \
	$(ALIB)(cdsgfa.o) \
	$(ALIB)(cdsgfatxt.o) \
	$(ALIB)(cdsgfill.o) \
	$(ALIB)(cdsinit.o) \
	$(ALIB)(cdsjet.o) \
	$(ALIB)(cdsline.o) \
	$(ALIB)(cdslist.o) \
	$(ALIB)(cdsmatch.o) \
	$(ALIB)(cdsress.o) \
	$(ALIB)(cdsrtbl.o) \
	$(ALIB)(cdsscal.o) \
	$(ALIB)(cdsscol.o) \
	$(ALIB)(cdssfill.o) \
	$(ALIB)(cdssig.o) \
	$(ALIB)(cdssymb.o) \
	$(ALIB)(cdstc.o) \
	$(ALIB)(cdstca.o) \
	$(ALIB)(cdstext.o) \
	$(ALIB)(cdstrack.o) \
	$(ALIB)(cdsvol.o) \
	$(ALIB)(cdswbox.o) \
	$(ALIB)(cdsccftxt.o) \
	$(ALIB)(cdswind.o)
 
GDIR = cds

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

