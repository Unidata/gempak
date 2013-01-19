#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(wocnty.o) \
	$(ALIB)(wodcod.o) \
	$(ALIB)(wodecd.o) \
	$(ALIB)(wohdln.o) \
	$(ALIB)(woout.o) \
	$(ALIB)(wosgmt.o) \
	$(ALIB)(wowfo.o)
 
GDIR = wo

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

