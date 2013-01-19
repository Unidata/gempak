#
#	Makefile for GEMPAK 6.0
#		Builds gpltdev.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gpltdev.a

FOBJS = \
	$(ALIB)(dendd.o) \
	$(ALIB)(dinita.o) \
	$(ALIB)(dinitp.o) \
	$(ALIB)(dqdchr.o)

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

