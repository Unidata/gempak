#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(sidcod.o) \
	$(ALIB)(sidecd.o) \
	$(ALIB)(sigemp.o) \
	$(ALIB)(sighdr.o) \
	$(ALIB)(sigrpt.o) \
	$(ALIB)(sigtim.o) \
	$(ALIB)(sitmst.o)
 
GDIR = si

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

