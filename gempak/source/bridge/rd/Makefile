#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(rdcdat.o) \
	$(ALIB)(rddcod.o) \
	$(ALIB)(rddcpm.o) \
	$(ALIB)(rdfctm.o) \
	$(ALIB)(rdgdte.o) \
	$(ALIB)(rdgitm.o) \
	$(ALIB)(rdglin.o) \
	$(ALIB)(rdgsgt.o) \
	$(ALIB)(rdgzon.o) \
	$(ALIB)(rdidat.o) \
	$(ALIB)(rdmv12.o) \
	$(ALIB)(rdrngd.o) \
	$(ALIB)(rdsynt.o) \
	$(ALIB)(rdtrce.o) \
	$(ALIB)(rdxfhr.o)
 
GDIR = rd

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

