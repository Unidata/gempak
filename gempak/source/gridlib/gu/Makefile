#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(gcboxx.o) \
	$(ALIB)(gcfill.o) \
	$(ALIB)(gclgrn.o) \
	$(ALIB)(gstrml.o)

FOBJS = \
	$(ALIB)(cadjst.o) \
	$(ALIB)(ccboxt.o) \
	$(ALIB)(ccchck.o) \
	$(ALIB)(cccrss.o) \
	$(ALIB)(ccdraw.o) \
	$(ALIB)(ccgtsb.o) \
	$(ALIB)(cclip.o) \
	$(ALIB)(ccoeff.o) \
	$(ALIB)(ccplot.o) \
	$(ALIB)(ccstrt.o) \
	$(ALIB)(clbbox.o) \
	$(ALIB)(cldriv.o) \
	$(ALIB)(csmthn.o) \
	$(ALIB)(ctrang.o) \
	$(ALIB)(ffdriv.o) \
	$(ALIB)(ffplot.o) \
	$(ALIB)(fgtbox.o) \
	$(ALIB)(fgtcor.o) \
	$(ALIB)(fgtsub.o) \
	$(ALIB)(filbox.o) \
	$(ALIB)(gcboxxf.o) \
	$(ALIB)(gcfillf.o) \
	$(ALIB)(gclgrnf.o) \
	$(ALIB)(gqcntr.o) \
	$(ALIB)(gscntr.o) \
	$(ALIB)(gstrmlf.o) \
	$(ALIB)(strmln.o)
 
GDIR = gu

#

.PRECIOUS: $(ALIB)

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

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(OS_LIB)/textlib.a $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(OS_LIB)/libgpc.a $(NETCDF) $(ZLIB) $(RSLLIB) $(LIBBZ2) 
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)
