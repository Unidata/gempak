#
#	Makefile for GEMPAK 6.0
#		Builds GDRADR
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(gdctbl.o) \
	$(ALIB)(gplutf.o) \
	$(ALIB)(im_nids_text_output.o)

COBJS = \
	$(ALIB)(im_nids_alpha_graphic.o) \
	$(ALIB)(im_nids_driver.o) \
	$(ALIB)(im_nids_packet_codes.o) \
	$(ALIB)(im_nids_symbol.o) \
	$(ALIB)(im_nids_tabular.o) \
	$(ALIB)(xytolatlon.o)

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

