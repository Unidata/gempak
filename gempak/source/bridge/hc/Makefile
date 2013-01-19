#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(hcctim.o) \
	$(ALIB)(hcdcod.o) \
	$(ALIB)(hcdecd.o) \
	$(ALIB)(hcfvld.o) \
	$(ALIB)(hcghdr.o) \
	$(ALIB)(hcout.o) \
	$(ALIB)(hcquad.o) \
	$(ALIB)(hcspqd.o)
 
GDIR = hc

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

