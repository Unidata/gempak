#
#	Makefile for GEMPAK 6.0
#		Builds appl.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(APPL)

FOBJS = \
	$(ALIB)(gclear.o) \
	$(ALIB)(gclosp.o) \
	$(ALIB)(gclpnl.o) \
	$(ALIB)(gegrp.o) \
	$(ALIB)(genanm.o) \
	$(ALIB)(geplot.o) \
	$(ALIB)(ggsave.o) \
	$(ALIB)(ggtpnt.o) \
	$(ALIB)(gloopc.o) \
	$(ALIB)(gmesg.o) \
	$(ALIB)(gqdev.o) \
	$(ALIB)(groam.o) \
	$(ALIB)(gsdeva.o) \
	$(ALIB)(gsgrp.o) \
	$(ALIB)(gsgtgn.o) \
	$(ALIB)(gslwin.o) \
	$(ALIB)(gsplot.o) \
	$(ALIB)(gsroam.o) \
	$(ALIB)(gstanm.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a : 
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o 

