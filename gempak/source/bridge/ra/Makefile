#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(rachck.o) \
	$(ALIB)(raclds.o) \
	$(ALIB)(raclev.o) \
	$(ALIB)(racsyb.o) \
	$(ALIB)(radecd.o) \
	$(ALIB)(ragfld.o) \
	$(ALIB)(ragrpt.o) \
	$(ALIB)(rap24i.o) \
	$(ALIB)(raptda.o) \
	$(ALIB)(raptnd.o) \
	$(ALIB)(rarhdr.o) \
	$(ALIB)(rarmks.o) \
	$(ALIB)(rartim.o) \
	$(ALIB)(rasnow.o) \
	$(ALIB)(ratmpx.o) \
	$(ALIB)(ratmst.o) \
	$(ALIB)(ravsby.o) \
	$(ALIB)(rawind.o) \
	$(ALIB)(rawthr.o)
 
GDIR = ra

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

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

