#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gqmmap.o) \
	$(ALIB)(gqmmgn.o) \
	$(ALIB)(gqmprj.o) \
	$(ALIB)(gsmmap.o) \
	$(ALIB)(gsmmgn.o) \
	$(ALIB)(gsmprj.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

