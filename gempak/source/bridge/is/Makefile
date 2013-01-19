#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(isarea.o) \
	$(ALIB)(isbdln.o) \
	$(ALIB)(iscbnd.o) \
	$(ALIB)(iscent.o) \
	$(ALIB)(ischng.o) \
	$(ALIB)(iscnar.o) \
	$(ALIB)(iscnda.o) \
	$(ALIB)(iscnph.o) \
	$(ALIB)(iscnpr.o) \
	$(ALIB)(iscnrg.o) \
	$(ALIB)(iscntm.o) \
	$(ALIB)(isdcod.o) \
	$(ALIB)(isdecd.o) \
	$(ALIB)(iseg2d.o) \
	$(ALIB)(isegao.o) \
	$(ALIB)(isegar.o) \
	$(ALIB)(isegca.o) \
	$(ALIB)(isegfd.o) \
	$(ALIB)(iseggy.o) \
	$(ALIB)(isegmv.o) \
	$(ALIB)(isegpr.o) \
	$(ALIB)(isegtm.o) \
	$(ALIB)(isegts.o) \
	$(ALIB)(isesol.o) \
	$(ALIB)(isflvl.o) \
	$(ALIB)(isintx.o) \
	$(ALIB)(islorl.o) \
	$(ALIB)(isltln.o) \
	$(ALIB)(ismov.o) \
	$(ALIB)(isobfc.o) \
	$(ALIB)(isosol.o) \
	$(ALIB)(isothr.o) \
	$(ALIB)(isotpr.o) \
	$(ALIB)(isout.o) \
	$(ALIB)(isphen.o) \
	$(ALIB)(isprmb.o) \
	$(ALIB)(istchu.o) \
	$(ALIB)(istime.o) \
	$(ALIB)(ists.o) \
	$(ALIB)(isturb.o) \
	$(ALIB)(isva.o) \
	$(ALIB)(isvloc.o)

GDIR = is

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

