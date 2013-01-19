#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(mrchkw.o) \
	$(ALIB)(mrcond.o) \
	$(ALIB)(mrintp.o) \
	$(ALIB)(mrintz.o) \
	$(ALIB)(mrmand.o) \
	$(ALIB)(mrmanw.o) \
	$(ALIB)(mrmhgt.o) \
	$(ALIB)(mrmiss.o) \
	$(ALIB)(mrordr.o) \
	$(ALIB)(mrpwnd.o) \
	$(ALIB)(mrscmz.o) \
	$(ALIB)(mrsigt.o) \
	$(ALIB)(mrsigw.o) \
	$(ALIB)(mrsrfc.o) \
	$(ALIB)(mrtrop.o) \
	$(ALIB)(mruadt.o)
 
GDIR = mr

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

