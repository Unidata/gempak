#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(wccnty.o) \
	$(ALIB)(wcdcod.o) \
	$(ALIB)(wcdecd.o) \
	$(ALIB)(wcghdr.o) \
	$(ALIB)(wcout.o) \
	$(ALIB)(wcvtec.o)
 
GDIR = wc

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

