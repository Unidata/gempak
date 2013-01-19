#
#	Makefile for GEMPAK 6.0
#		Builds device.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(DEVICE)

FOBJS = \
	$(ALIB)(dclear.o) \
	$(ALIB)(dclosp.o) \
	$(ALIB)(dclpnl.o) \
	$(ALIB)(degrp.o) \
	$(ALIB)(denanm.o) \
	$(ALIB)(deplot.o) \
	$(ALIB)(dgsave.o) \
	$(ALIB)(dgtpnt.o) \
	$(ALIB)(dloopc.o) \
	$(ALIB)(dmesg.o) \
	$(ALIB)(dqdev.o) \
	$(ALIB)(droam.o) \
	$(ALIB)(dsgrp.o) \
	$(ALIB)(dsgtgn.o) \
	$(ALIB)(dslwin.o) \
	$(ALIB)(dsplot.o) \
	$(ALIB)(dsroam.o) \
	$(ALIB)(dstanm.o)

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

