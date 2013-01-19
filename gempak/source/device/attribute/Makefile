#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(DEVICE)

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

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

