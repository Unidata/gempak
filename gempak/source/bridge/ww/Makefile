#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

COBJS = \
	$(ALIB)(wwcrnr.o)

FOBJS = \
	$(ALIB)(wwaxpt.o) \
	$(ALIB)(wwctim.o) \
	$(ALIB)(wwdcod.o) \
	$(ALIB)(wwdecd.o) \
	$(ALIB)(wwout.o) \
	$(ALIB)(wwus40.o) \
	$(ALIB)(wwus8.o) \
	$(ALIB)(wwwnum.o)
 
GDIR = ww

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

