#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(shpbhdr.o) \
	$(ALIB)(shpcbnd.o) \
	$(ALIB)(shpcmap.o) \
	$(ALIB)(shpcmbn.o) \
	$(ALIB)(shpctbl.o) \
	$(ALIB)(shpdrec.o) \
	$(ALIB)(shpgctr.o) \
	$(ALIB)(shpgkey.o) \
	$(ALIB)(shpjoin.o) \
	$(ALIB)(shpmmem.o) \
	$(ALIB)(shpmtyp.o) \
	$(ALIB)(shprdbf.o) \
	$(ALIB)(shprdbh.o) \
	$(ALIB)(shprshp.o) \
	$(ALIB)(shprshx.o) \
	$(ALIB)(shpsplt.o) \
	$(ALIB)(shpstrip.o) \
	$(ALIB)(shpthin.o) \
	$(ALIB)(shputil.o) \
	$(ALIB)(shpwfld.o) \
	$(ALIB)(shpwprt.o) \
	$(ALIB)(shpwrec.o)

 
GDIR = shp

.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

