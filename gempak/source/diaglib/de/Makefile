#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(decprb.o) \
	$(ALIB)(decval.o) \
	$(ALIB)(dedriv.o) \
	$(ALIB)(deinit.o) \
	$(ALIB)(dembr1.o) \
	$(ALIB)(demode.o) \
	$(ALIB)(demset.o) \
	$(ALIB)(depfpn.o) \
	$(ALIB)(deprcntl.o) \
	$(ALIB)(derset.o) \
	$(ALIB)(desave.o) \
	$(ALIB)(desavg.o) \
	$(ALIB)(descan.o) \
	$(ALIB)(desmax.o) \
	$(ALIB)(desmin.o) \
	$(ALIB)(desrng.o) \
	$(ALIB)(dessprd.o) \
	$(ALIB)(dessum.o) \
	$(ALIB)(deswsprd.o) \
	$(ALIB)(devavg.o) \
	$(ALIB)(devsprd.o) \
	$(ALIB)(devwsprd.o)

GDIR = de

.PRECIOUS: $(ALIB)


all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<



clean:
	$(RM) *.o 

tlink : test$(GDIR).f 
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
