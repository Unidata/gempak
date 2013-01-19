#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gqggrf.o) \
	$(ALIB)(gqgmap.o) \
	$(ALIB)(gqgprj.o) \
	$(ALIB)(gsggrf.o) \
	$(ALIB)(gsgmap.o) \
	$(ALIB)(gsgprj.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

