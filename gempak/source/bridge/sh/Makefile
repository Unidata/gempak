#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

COBJS = \
	$(ALIB)(shnclin.o) \
	$(ALIB)(shndfhr.o)

FOBJS = \
	$(ALIB)(shn5004.o) \
	$(ALIB)(shnbfsq.o) \
	$(ALIB)(shnbufr.o) \
	$(ALIB)(shndcod.o) \
	$(ALIB)(shngemp.o) \
	$(ALIB)(shnglin.o) \
	$(ALIB)(shnifiv.o) \
	$(ALIB)(shnifpt.o) \
	$(ALIB)(shnintf.o) \
	$(ALIB)(shnstor.o) \
	$(ALIB)(shntdim.o)

 
GDIR = sh

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

