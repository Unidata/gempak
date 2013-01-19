#
#	Makefile for GEMPAK 6.0
#		Builds libaodt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libAODT_v72.a

COBJS = \
	$(ALIB)(odtauto.o) \
	$(ALIB)(odtdata.o) \
	$(ALIB)(odtfft.o) \
	$(ALIB)(odtfuncs.o) \
	$(ALIB)(odthistory.o) \
	$(ALIB)(odtintensity.o) \
	$(ALIB)(odtoutput.o) \
	$(ALIB)(odtscene.o) \
	$(ALIB)(odttopo.o) \
	$(ALIB)(odtwinds.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

