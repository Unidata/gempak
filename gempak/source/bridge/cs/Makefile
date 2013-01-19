#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(cscbnd.o) \
	$(ALIB)(csconv.o) \
	$(ALIB)(csctim.o) \
	$(ALIB)(csdcod.o) \
	$(ALIB)(csdecd.o) \
	$(ALIB)(csflvl.o) \
	$(ALIB)(cshdr.o) \
	$(ALIB)(csobnd.o) \
	$(ALIB)(csotlk.o) \
	$(ALIB)(csout.o)
 
GDIR = cs

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

