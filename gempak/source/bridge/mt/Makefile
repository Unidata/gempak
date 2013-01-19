#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(mtaltm.o) \
	$(ALIB)(mtauto.o) \
	$(ALIB)(mtceil.o) \
	$(ALIB)(mtcltp.o) \
	$(ALIB)(mtcncltp.o) \
	$(ALIB)(mtdate.o) \
	$(ALIB)(mtdcod.o) \
	$(ALIB)(mtdecd.o) \
	$(ALIB)(mtdrop.o) \
	$(ALIB)(mtdsao.o) \
	$(ALIB)(mtelog.o) \
	$(ALIB)(mtfind.o) \
	$(ALIB)(mtgemp.o) \
	$(ALIB)(mtgrpt.o) \
	$(ALIB)(mtifpt.o) \
	$(ALIB)(mtifsp.o) \
	$(ALIB)(mtinit.o) \
	$(ALIB)(mtintf.o) \
	$(ALIB)(mtmmt6.o) \
	$(ALIB)(mtnext.o) \
	$(ALIB)(mtpkwd.o) \
	$(ALIB)(mtpos2.o) \
	$(ALIB)(mtpost.o) \
	$(ALIB)(mtprrm.o) \
	$(ALIB)(mtpten.o) \
	$(ALIB)(mtrmks.o) \
	$(ALIB)(mtrmmx.o) \
	$(ALIB)(mtrvr4.o) \
	$(ALIB)(mtshdr.o) \
	$(ALIB)(mtsky6.o) \
	$(ALIB)(mtslp.o) \
	$(ALIB)(mtsnow.o) \
	$(ALIB)(mtsspl.o) \
	$(ALIB)(mttdpt.o) \
	$(ALIB)(mttprm.o) \
	$(ALIB)(mtvisb.o) \
	$(ALIB)(mtvwnd.o) \
	$(ALIB)(mtwea3.o) \
	$(ALIB)(mtwind.o) \
	$(ALIB)(mtwshf.o)
 
GDIR = mt

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

