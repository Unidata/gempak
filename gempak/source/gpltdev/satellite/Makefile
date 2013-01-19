#
#	Makefile for GEMPAK 6.0
#		Builds gpltdev.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLTDEV)

FOBJS = \
	$(ALIB)(dsatim.o) \
	$(ALIB)(dsatpx.o) \
	$(ALIB)(dsicmn.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

