#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(dpendp.o) \
	$(ALIB)(dpfile.o) \
	$(ALIB)(dppack.o) \
	$(ALIB)(dppdec.o) \
	$(ALIB)(dppdif.o) \
	$(ALIB)(dppgrb.o) \
	$(ALIB)(dpsetp.o) \
	$(ALIB)(dpterm.o) \
	$(ALIB)(dpudif.o) \
	$(ALIB)(dpugrb.o) \
	$(ALIB)(dpugb2.o) \
	$(ALIB)(dpunmc.o) \
	$(ALIB)(dpunpk.o)
 
GDIR = dp

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

tlink : test$(GDIR).f
	$(COMPILE.f) test$(GDIR).f
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) $(LIBJASPER) $(PNGLIB) \
		$(ZLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

