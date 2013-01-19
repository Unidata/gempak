#
#	Makefile for GEMPAK 6.0
#		Builds bridge.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(BRIDGE)

FOBJS = \
	$(ALIB)(tfctim.o) \
	$(ALIB)(tfdcod.o) \
	$(ALIB)(tfdecd.o) \
	$(ALIB)(tffcin.o) \
	$(ALIB)(tfgftm.o) \
	$(ALIB)(tfgitm.o) \
	$(ALIB)(tfgrpt.o) \
	$(ALIB)(tfgvtm.o) \
	$(ALIB)(tfvnum.o) \
	$(ALIB)(tfmobv.o) \
	$(ALIB)(tfshdr.o) \
	$(ALIB)(tfsy24.o) \
	$(ALIB)(tfvalt.o)
 
GDIR = tf

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

