#
#	Makefile for GEMPAK7
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libGAMET.a

COBJS = \
	$(ALIB)(gamet.o)
 

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

