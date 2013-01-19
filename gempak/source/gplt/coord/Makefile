#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(ud2cyl.o) \
	$(ALIB)(udmbnd.o) \
	$(ALIB)(updazm.o) \
	$(ALIB)(updcon.o) \
	$(ALIB)(updcyl.o) \
	$(ALIB)(upddxy.o) \
	$(ALIB)(updgdg.o) \
	$(ALIB)(updgdi.o) \
	$(ALIB)(updgdm.o) \
	$(ALIB)(updgrf.o) \
	$(ALIB)(updgxy.o) \
	$(ALIB)(updmap.o) \
	$(ALIB)(updmer.o) \
	$(ALIB)(updnxy.o) \
	$(ALIB)(updpxy.o) \
	$(ALIB)(updsxy.o) \
	$(ALIB)(updvxy.o) \
	$(ALIB)(upwndw.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

