#
#	Makefile for GEMPAK 6.0
#		Builds gpltdev.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLTDEV)

COBJS = 

FOBJS = \
	$(ALIB)(dqclrs.o) \
	$(ALIB)(dqcolr.o) \
	$(ALIB)(dqcomp.o) \
	$(ALIB)(dsbrgb.o) \
	$(ALIB)(dscint.o) \
	$(ALIB)(dsclr2.o) \
	$(ALIB)(dscnam.o) \
	$(ALIB)(dscolb.o) \
	$(ALIB)(dscolr.o) \
	$(ALIB)(dscrgb.o) \
	$(ALIB)(dsctbl.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

