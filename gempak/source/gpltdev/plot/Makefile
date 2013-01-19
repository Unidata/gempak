#
#	Makefile for GEMPAK 6.0
#		Builds gpltdev.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/gpltdev.a

FOBJS = \
	$(ALIB)(darc.o) \
	$(ALIB)(darrw.o) \
	$(ALIB)(dbarb.o) \
	$(ALIB)(dcircl.o) \
	$(ALIB)(dcmbo.o) \
	$(ALIB)(dctyp.o) \
	$(ALIB)(ddarr.o) \
	$(ALIB)(dfill.o) \
	$(ALIB)(dfrnt.o) \
	$(ALIB)(dhash.o) \
	$(ALIB)(dicng.o) \
	$(ALIB)(dline.o) \
	$(ALIB)(dlogo.o) \
	$(ALIB)(dmark.o) \
	$(ALIB)(dptnd.o) \
	$(ALIB)(dpwth.o) \
	$(ALIB)(dsky.o) \
	$(ALIB)(dspcl.o) \
	$(ALIB)(dspln.o) \
	$(ALIB)(dtext.o) \
	$(ALIB)(dtextc.o) \
	$(ALIB)(dticmk.o) \
	$(ALIB)(dturb.o) \
	$(ALIB)(dtxsy.o) \
	$(ALIB)(dwthr.o)

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(FOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o 

