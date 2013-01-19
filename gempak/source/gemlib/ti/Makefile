#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

LOBJS = \
	$(ALIB)(ctimtch.o) \
	$(ALIB)(ctirseq.o) \
	$(ALIB)(ctitmln.o) \
	$(ALIB)(ctiyyyy.o) \
	$(ALIB)(tiaddd.o) \
	$(ALIB)(tiaddm.o) \
	$(ALIB)(tic2i.o) \
	$(ALIB)(ticcnt.o) \
	$(ALIB)(ticdtm.o) \
	$(ALIB)(tictoi.o) \
	$(ALIB)(tidaym.o) \
	$(ALIB)(tidayw.o) \
	$(ALIB)(tidifd.o) \
	$(ALIB)(tidiff.o) \
	$(ALIB)(tidspl.o) \
	$(ALIB)(tidst.o) \
	$(ALIB)(tidtm4.o) \
	$(ALIB)(tielcl.o) \
	$(ALIB)(tifind.o) \
	$(ALIB)(tiform.o) \
	$(ALIB)(tii2c.o) \
	$(ALIB)(tiidtm.o) \
	$(ALIB)(tiitoc.o) \
	$(ALIB)(tiitoj.o) \
	$(ALIB)(tijtoi.o) \
	$(ALIB)(tilocl.o) \
	$(ALIB)(timdif.o) \
	$(ALIB)(timtch.o) \
	$(ALIB)(tirang.o) \
	$(ALIB)(tirseq.o) \
	$(ALIB)(tisort.o) \
	$(ALIB)(tistan.o) \
	$(ALIB)(tistnt.o) \
	$(ALIB)(tisubd.o) \
	$(ALIB)(tisubm.o) \
	$(ALIB)(titmln.o) \
	$(ALIB)(titzdf.o) \
	$(ALIB)(tiyy24.o) \
	$(ALIB)(tiyymd.o) \
	$(ALIB)(tiyyyy.o)
 
GDIR = ti

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(LOBJS)
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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(CGEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
