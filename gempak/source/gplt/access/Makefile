#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(gendp.o) \
	$(ALIB)(ginit.o) \
	$(ALIB)(ginitp.o) \
	$(ALIB)(gqmode.o) \
	$(ALIB)(gsmode.o)

.PRECIOUS: $(ALIB)


all : $(ALIB) alts

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


alts:
	@cd alt;  echo "making alt library in `pwd`" ; \
	$(MAKE) $(MFLAGS)

clean:
	$(RM) *.o 
	@cd alt;  echo "making clean in alt dir"; $(MAKE) $(MFLAGS) $@

