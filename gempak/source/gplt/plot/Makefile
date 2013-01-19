#
#	Makefile for GEMPAK 6.0
#		Builds gplt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GPLT)

FOBJS = \
	$(ALIB)(garc.o) \
	$(ALIB)(garrw.o) \
	$(ALIB)(gbarb.o) \
	$(ALIB)(gcircl.o) \
	$(ALIB)(gcmbo.o) \
	$(ALIB)(gctyp.o) \
	$(ALIB)(gdarr.o) \
	$(ALIB)(gfill.o) \
	$(ALIB)(gfpro.o) \
	$(ALIB)(gfrnt.o) \
	$(ALIB)(ghash.o) \
	$(ALIB)(gicng.o) \
	$(ALIB)(gline.o) \
	$(ALIB)(glogo.o) \
	$(ALIB)(gmark.o) \
	$(ALIB)(gptnd.o) \
	$(ALIB)(gpwth.o) \
	$(ALIB)(gsky.o) \
	$(ALIB)(gspcl.o) \
	$(ALIB)(gspln.o) \
	$(ALIB)(gtext.o) \
	$(ALIB)(gtextc.o) \
	$(ALIB)(gturb.o) \
	$(ALIB)(gtxsy.o) \
	$(ALIB)(gwthr.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

