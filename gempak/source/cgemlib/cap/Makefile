#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(capplclear.o) \
	$(ALIB)(capplclrplace.o) \
	$(ALIB)(capplgetcenter.o) \
	$(ALIB)(capplgetdistincr.o) \
	$(ALIB)(capplgetdistoffset.o) \
	$(ALIB)(capplgetid.o) \
	$(ALIB)(capplgetincntr.o) \
	$(ALIB)(capplgetline.o) \
	$(ALIB)(capplgetmode.o) \
	$(ALIB)(capplgetoffset.o) \
	$(ALIB)(capplgetpcntr.o) \
	$(ALIB)(capplgetplaced.o) \
	$(ALIB)(capplgetref.o) \
	$(ALIB)(capplgetsides.o) \
	$(ALIB)(capplgettries.o) \
	$(ALIB)(capplsetcenter.o) \
	$(ALIB)(capplsetdist.o) \
	$(ALIB)(capplsetmode.o) \
	$(ALIB)(capplsetpcntr.o) \
	$(ALIB)(capplsetref.o) \
	$(ALIB)(capplsetsides.o) \
	$(ALIB)(capplsettries.o) \
	$(ALIB)(cappsaddpl.o) \
	$(ALIB)(cappsclear.o) \
	$(ALIB)(cappsclrplaced.o) \
	$(ALIB)(cappsdel.o) \
	$(ALIB)(cappsdelpl.o) \
	$(ALIB)(cappsfindpl.o) \
	$(ALIB)(cappsgetarea.o) \
	$(ALIB)(cappsgetpl.o) \
	$(ALIB)(cappsgetplarea.o) \
	$(ALIB)(cappsgetplmaxdist.o) \
	$(ALIB)(cappsiterinit.o) \
	$(ALIB)(cappsiternext.o) \
	$(ALIB)(cappsmarkint.o) \
	$(ALIB)(cappsnew.o) \
	$(ALIB)(cappsnewpl.o) \
	$(ALIB)(cappsplace.o) \
	$(ALIB)(cappssetarea.o) \
	$(ALIB)(cappssetdist.o) \
	$(ALIB)(cappssetincr.o)

 
GDIR = cap

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

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

