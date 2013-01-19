#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(ctgcftm.o) \
	$(ALIB)(ctgfull.o) \
	$(ALIB)(ctgitoc.o) \
	$(ALIB)(ctgrinc.o) \
	$(ALIB)(ctgrnoi.o) \
	$(ALIB)(ctgvald.o) \
	$(ALIB)(ctgvi2f.o) \

FOBJS = \
	$(ALIB)(tgcftm.o) \
	$(ALIB)(tgcrinc.o) \
	$(ALIB)(tgctoi.o) \
	$(ALIB)(tgdiff.o) \
	$(ALIB)(tgdual.o) \
	$(ALIB)(tgfind.o) \
	$(ALIB)(tgflst.o) \
	$(ALIB)(tgftoi.o) \
	$(ALIB)(tgfull.o) \
	$(ALIB)(tgginc.o) \
	$(ALIB)(tgiftm.o) \
	$(ALIB)(tgitoc.o) \
	$(ALIB)(tgitof.o) \
	$(ALIB)(tgmtch.o) \
	$(ALIB)(tgqrng.o) \
	$(ALIB)(tgrang.o) \
	$(ALIB)(tgrinc.o) \
	$(ALIB)(tgrincw.o) \
	$(ALIB)(tgrnoi.o) \
	$(ALIB)(tgrnoiw.o) \
	$(ALIB)(tgvald.o) \
	$(ALIB)(tgvi2f.o) \
	$(ALIB)(tgvtof.o) \
	$(ALIB)(tgyymd.o)
 
GDIR = tg

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS) $(FOBJS)
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

