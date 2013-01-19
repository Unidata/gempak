#
#	Makefile for GEMPAK 6.0
#		Builds syslib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(SYSLIB)

COBJS = \
	$(ALIB)(ccheck.o) \
	$(ALIB)(cendmq.o) \
	$(ALIB)(cgetmq.o) \
	$(ALIB)(ciproc.o) \
	$(ALIB)(cigetq.o) \
	$(ALIB)(cnsleep.o) \
	$(ALIB)(crecv.o) \
	$(ALIB)(csend.o) \
	$(ALIB)(csfree.o) \
	$(ALIB)(csgrab.o) \
	$(ALIB)(csproc.o) 

FOBJS = \
	$(ALIB)(gget.o) \
	$(ALIB)(ggetb.o) \
	$(ALIB)(ggetc.o) \
	$(ALIB)(ggetnw.o) \
	$(ALIB)(ggetr.o) \
	$(ALIB)(giproc.o) \
	$(ALIB)(gput.o) \
	$(ALIB)(gputb.o) \
	$(ALIB)(gputc.o) \
	$(ALIB)(gputr.o) \
	$(ALIB)(grecv.o) \
	$(ALIB)(gsend.o) \
	$(ALIB)(gsfree.o) \
	$(ALIB)(gsgrab.o) \
	$(ALIB)(gsproc.o)


.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(FOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a : 
	$(CC) $(CFLAGS) -c $<
.f.a : 
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

