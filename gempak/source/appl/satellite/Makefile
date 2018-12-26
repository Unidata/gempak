#
#	Makefile for GEMPAK 6.0
#		Builds appl.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(APPL)

FOBJS = \
	$(ALIB)(gqsatn.o) \
	$(ALIB)(gsatim.o) \
	$(ALIB)(gsatmg.o) \
	$(ALIB)(gsatmg4.o) \
	$(ALIB)(gsatpx.o) \
	$(ALIB)(gsicmn.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o 

