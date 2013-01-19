#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gaaxis.o) \
	$(ALIB)(gdaxis.o) \
	$(ALIB)(gdaxlb.o) \
	$(ALIB)(gdpaxs.o) \
	$(ALIB)(gqgmgn.o) \
	$(ALIB)(gqgraf.o) \
	$(ALIB)(gsgmgn.o) \
	$(ALIB)(gsgraf.o) \
	$(ALIB)(gtick.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

