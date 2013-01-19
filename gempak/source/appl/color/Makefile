#
#	Makefile for GEMPAK 6.0
#		Builds appl.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(APPL)

FOBJS = \
	$(ALIB)(gqclr2.o) \
	$(ALIB)(gqclrs.o) \
	$(ALIB)(gqcolr.o) \
	$(ALIB)(gqcomp.o) \
	$(ALIB)(gqncol.o) \
	$(ALIB)(gsbrgb.o) \
	$(ALIB)(gscint.o) \
	$(ALIB)(gsclr2.o) \
	$(ALIB)(gscnam.o) \
	$(ALIB)(gscolb.o) \
	$(ALIB)(gscolr.o) \
	$(ALIB)(gscrgb.o) \
	$(ALIB)(gsctbl.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o 

