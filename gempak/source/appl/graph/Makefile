#
#	Makefile for GEMPAK 6.0
#		Builds appl.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(APPL)

FOBJS = \
	$(ALIB)(gaaxis.o) \
	$(ALIB)(gdaxis.o) \
	$(ALIB)(gqgmgn.o) \
	$(ALIB)(gqgraf.o) \
	$(ALIB)(gsgmgn.o) \
	$(ALIB)(gsgraf.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a : 
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o 

