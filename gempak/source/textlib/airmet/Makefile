#
#	Makefile for GEMPAK 6.0
#		Builds cgemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/textlib.a

COBJS = \
	$(ALIB)(afapplyattr.o) \
	$(ALIB)(afarearules.o) \
	$(ALIB)(afbounds.o) \
	$(ALIB)(afconditions.o) \
	$(ALIB)(afcreate.o) \
	$(ALIB)(afcrtfbba.o) \
	$(ALIB)(afdisplay.o) \
	$(ALIB)(affmtxml.o) \
	$(ALIB)(affrom.o) \
	$(ALIB)(affzlvl.o) \
	$(ALIB)(afgetairmetxml.o) \
	$(ALIB)(afgroups.o) \
	$(ALIB)(afreducepts.o) \
	$(ALIB)(afutils.o)

LIBINC = \
	-L$(OS_LIB) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(CGEMLIB) \
	-lgpc \
	$(GPLT_NC) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(GEMLIB) \
	-lxml2 -lxslt

GDIR = airmet

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

