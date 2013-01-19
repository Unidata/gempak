#
#	Makefile for GEMPAK 6.0
#		Builds gpltdev.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gpltdev.a

FOBJS = \
	$(ALIB)(dqdatt.o) \
	$(ALIB)(dqlpat.o) \
	$(ALIB)(dsarrw.o) \
	$(ALIB)(dsbarb.o) \
	$(ALIB)(dscmbo.o) \
	$(ALIB)(dsctyp.o) \
	$(ALIB)(dsdarr.o) \
	$(ALIB)(dsdash.o) \
	$(ALIB)(dsdatt.o) \
	$(ALIB)(dsfill.o) \
	$(ALIB)(dsfrnt.o) \
	$(ALIB)(dshash.o) \
	$(ALIB)(dsicng.o) \
	$(ALIB)(dsline.o) \
	$(ALIB)(dslpat.o) \
	$(ALIB)(dsmrkr.o) \
	$(ALIB)(dsptnd.o) \
	$(ALIB)(dspwth.o) \
	$(ALIB)(dsrduc.o) \
	$(ALIB)(dssky.o) \
	$(ALIB)(dssmth.o) \
	$(ALIB)(dsspcl.o) \
	$(ALIB)(dsspln.o) \
	$(ALIB)(dstext.o) \
	$(ALIB)(dsturb.o) \
	$(ALIB)(dswthr.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS) 
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o


.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

