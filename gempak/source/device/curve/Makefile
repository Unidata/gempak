#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(DEVICE)

FOBJS = \
	$(ALIB)(dcgaus.o) \
	$(ALIB)(despln.o) \
	$(ALIB)(dgauss.o) \
	$(ALIB)(dmatrx.o) \
	$(ALIB)(dsplin.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

