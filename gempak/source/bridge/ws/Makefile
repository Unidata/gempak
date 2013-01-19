#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(wsdcod.o) \
	$(ALIB)(wsdecd.o) \
	$(ALIB)(wshdln.o) \
	$(ALIB)(wsout.o) \
	$(ALIB)(wssgmt.o) \
	$(ALIB)(wszone.o)
 
GDIR = ws

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

