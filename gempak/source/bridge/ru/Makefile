#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(ruadjt.o) \
	$(ALIB)(rudecd.o) \
	$(ALIB)(rudrop.o) \
	$(ALIB)(rudrp1.o) \
	$(ALIB)(rudrp2.o) \
	$(ALIB)(rudupe.o) \
	$(ALIB)(rugbul.o) \
	$(ALIB)(rugfld.o) \
	$(ALIB)(rugrpt.o) \
	$(ALIB)(ruhght.o) \
	$(ALIB)(rumand.o) \
	$(ALIB)(rumlvl.o) \
	$(ALIB)(rummxw.o) \
	$(ALIB)(rumsfc.o) \
	$(ALIB)(rumtrp.o) \
	$(ALIB)(ruplvl.o) \
	$(ALIB)(ruppaa.o) \
	$(ALIB)(ruppbb.o) \
	$(ALIB)(ruppcc.o) \
	$(ALIB)(ruppdd.o) \
	$(ALIB)(ruppmw.o) \
	$(ALIB)(rupwnd.o) \
	$(ALIB)(rurtim.o) \
	$(ALIB)(rushdr.o) \
	$(ALIB)(rusigw.o) \
	$(ALIB)(rustmp.o) \
	$(ALIB)(rutemp.o) \
	$(ALIB)(rutmst.o) \
	$(ALIB)(ruttaa.o) \
	$(ALIB)(ruttbb.o) \
	$(ALIB)(ruttcc.o) \
	$(ALIB)(ruttdd.o) \
	$(ALIB)(ruwhgt.o) \
	$(ALIB)(ruwind.o)
 
GDIR = ru

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

