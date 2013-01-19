#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(DEVICE)

FOBJS = \
	$(ALIB)(iarc.o) \
	$(ALIB)(iarrw.o) \
	$(ALIB)(ibarb.o) \
	$(ALIB)(icirc.o) \
	$(ALIB)(iclip.o) \
	$(ALIB)(ictyp.o) \
	$(ALIB)(idash.o) \
	$(ALIB)(idraw.o) \
	$(ALIB)(ifill.o) \
	$(ALIB)(iicng.o) \
	$(ALIB)(ikink.o) \
	$(ALIB)(iline.o) \
	$(ALIB)(imark.o) \
	$(ALIB)(inoaa.o) \
	$(ALIB)(inws.o) \
	$(ALIB)(ipatln.o) \
	$(ALIB)(iptnd.o) \
	$(ALIB)(ipwth.o) \
	$(ALIB)(iquad.o) \
	$(ALIB)(isky.o) \
	$(ALIB)(ispcl.o) \
	$(ALIB)(ispln.o) \
	$(ALIB)(isymb.o) \
	$(ALIB)(itext1.o) \
	$(ALIB)(itext2.o) \
	$(ALIB)(itext3.o) \
	$(ALIB)(iticmk.o) \
	$(ALIB)(itrif.o) \
	$(ALIB)(iturb.o) \
	$(ALIB)(itxccf.o) \
	$(ALIB)(itxbox.o) \
	$(ALIB)(itxgfa.o) \
	$(ALIB)(itxmlv.o) \
	$(ALIB)(itxsy.o) \
	$(ALIB)(iwthr.o)

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

