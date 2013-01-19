#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gdrgrd.o) \
	$(ALIB)(gdrmap.o) \
	$(ALIB)(getmap.o) \
	$(ALIB)(gplbnd.o) \
	$(ALIB)(gqmfil.o) \
	$(ALIB)(gsmfil.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

