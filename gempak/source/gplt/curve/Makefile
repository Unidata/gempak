#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gcelin.o) \
	$(ALIB)(gcespl.o) \
	$(ALIB)(gcflin.o) \
	$(ALIB)(gcfpar.o) \
	$(ALIB)(gcfspl.o) \
	$(ALIB)(gctype.o) \
	$(ALIB)(gcurve.o) \
	$(ALIB)(gcyevl.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

