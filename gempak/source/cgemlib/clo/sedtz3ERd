#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(CGEMLIB)

COBJS = \
	$(ALIB)(clobgall.o) \
	$(ALIB)(clobgcent.o) \
	$(ALIB)(clobginfo.o) \
	$(ALIB)(clobgnext.o) \
	$(ALIB)(clobgrange.o) \
	$(ALIB)(clobinpoly.o) \
	$(ALIB)(cloblasso.o) \
	$(ALIB)(clobofile.o) \
	$(ALIB)(clobqtag.o) \
	$(ALIB)(clobrdrec.o) \
	$(ALIB)(clobsarea.o) \
	$(ALIB)(clobstag.o) \
	$(ALIB)(clobstype.o) \
	$(ALIB)(clocleanfmline.o) \
	$(ALIB)(cloclip.o) \
	$(ALIB)(cloclosest.o) \
	$(ALIB)(clocmpdir.o) \
	$(ALIB)(clocmpwds.o) \
	$(ALIB)(clocompass.o) \
	$(ALIB)(clodddec.o) \
	$(ALIB)(cloddenc.o) \
	$(ALIB)(clodirect.o) \
	$(ALIB)(clodist.o) \
	$(ALIB)(clodltln.o) \
	$(ALIB)(clofastates.o) \
	$(ALIB)(clofindcwa.o) \
	$(ALIB)(clofinddesc.o) \
	$(ALIB)(clofinddescw.o) \
	$(ALIB)(clofindmatch.o) \
	$(ALIB)(clofindnum.o) \
	$(ALIB)(clofindstn.o) \
	$(ALIB)(clofindstnw.o) \
	$(ALIB)(clofrom.o) \
	$(ALIB)(cloinit.o) \
	$(ALIB)(clolonin.o) \
	$(ALIB)(cloqformat.o) \
	$(ALIB)(cloqmxpts.o) \
	$(ALIB)(cloqnhot.o) \
	$(ALIB)(clordbnd.o) \
	$(ALIB)(clordstn.o) \
	$(ALIB)(cloreorder.o) \
	$(ALIB)(closnap.o) \
	$(ALIB)(closnaponept.o) \
	$(ALIB)(closortbnd.o) \
	$(ALIB)(closortstn.o) \
	$(ALIB)(clostngall.o) \
	$(ALIB)(clotclosest.o) \
	$(ALIB)(clotdirect.o) \
	$(ALIB)(clotgid.o) \
	$(ALIB)(clotgltln.o) \
	$(ALIB)(clotgnm.o) \
	$(ALIB)(clotgparm.o) \
	$(ALIB)(clotgst.o) \
	$(ALIB)(clotimes.o) \
	$(ALIB)(clotinltln.o) \
	$(ALIB)(clotinpoly.o) \
	$(ALIB)(clotmatch.o) \
	$(ALIB)(clotqbnd.o) \
	$(ALIB)(clotrack.o) \
	$(ALIB)(clowhich.o)
 
GDIR = clo

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

tlink : test$(GDIR).c
	$(COMPILE.c) test$(GDIR).c
	$(LINK.f) $(NOFORMAIN) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(CGEMLIB) $(APPL) $(SYSLIB) $(GEMLIB) -lm
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

