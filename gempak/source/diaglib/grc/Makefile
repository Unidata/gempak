#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(grcacol.o) \
	$(ALIB)(grcalgn.o) \
	$(ALIB)(grccnav.o) \
	$(ALIB)(grcdorg.o) \
	$(ALIB)(grcgeni.o) \
	$(ALIB)(grcgtim.o) \
	$(ALIB)(grclevl.o) \
	$(ALIB)(grcltln.o) \
	$(ALIB)(grcmbn2.o) \
	$(ALIB)(grcmnav.o) \
	$(ALIB)(grcpack.o) \
	$(ALIB)(grcplin.o) \
	$(ALIB)(grcploc.o) \
	$(ALIB)(grcrban.o) \
	$(ALIB)(grcrnav.o) \
	$(ALIB)(grcsetr.o) \
	$(ALIB)(grcsscl.o) \
	$(ALIB)(grcsnav.o) \
	$(ALIB)(grcstat.o) \
	$(ALIB)(grcsuba.o) \
	$(ALIB)(grcwgb2.o) \
	$(ALIB)(grcwnmc.o) \
	$(ALIB)(grcwtrm.o)
 
GDIR = grc

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
