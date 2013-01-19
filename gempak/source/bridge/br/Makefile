#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(brbnds.o) \
	$(ALIB)(brcmtn.o) \
	$(ALIB)(brcnty.o) \
	$(ALIB)(brdttm.o) \
	$(ALIB)(brgmsk.o) \
	$(ALIB)(brsky6.o) \
	$(ALIB)(brvald.o) \
	$(ALIB)(brvisb.o) \
	$(ALIB)(brvtec.o) \
	$(ALIB)(brwind.o)
 
GDIR = br

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

