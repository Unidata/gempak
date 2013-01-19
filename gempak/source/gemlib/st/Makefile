#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(stabbr.o) \
	$(ALIB)(stalnm.o) \
	$(ALIB)(statoe.o) \
	$(ALIB)(stc2c.o) \
	$(ALIB)(stc2i.o) \
	$(ALIB)(stc2r.o) \
	$(ALIB)(stcls2.o) \
	$(ALIB)(stclsl.o) \
	$(ALIB)(stclst.o) \
	$(ALIB)(stcrnd.o) \
	$(ALIB)(stcrnm.o) \
	$(ALIB)(stctoi.o) \
	$(ALIB)(stetoa.o) \
	$(ALIB)(stfind.o) \
	$(ALIB)(stflst.o) \
	$(ALIB)(stftlt.o) \
	$(ALIB)(stfwrd.o) \
	$(ALIB)(stgtst.o) \
	$(ALIB)(stilsr.o) \
	$(ALIB)(stilst.o) \
	$(ALIB)(stinch.o) \
	$(ALIB)(stinln.o) \
	$(ALIB)(stintg.o) \
	$(ALIB)(stistp.o) \
	$(ALIB)(stitoc.o) \
	$(ALIB)(stitos.o) \
	$(ALIB)(stlcuc.o) \
	$(ALIB)(stldsp.o) \
	$(ALIB)(stlstc.o) \
	$(ALIB)(stlstf.o) \
	$(ALIB)(stlsti.o) \
	$(ALIB)(stlstr.o) \
	$(ALIB)(stnarg.o) \
	$(ALIB)(stnocc.o) \
	$(ALIB)(stnull.o) \
	$(ALIB)(stnumb.o) \
	$(ALIB)(stnxts.o) \
	$(ALIB)(stopcl.o) \
	$(ALIB)(stpelt.o) \
	$(ALIB)(strang.o) \
	$(ALIB)(strlch.o) \
	$(ALIB)(strlst.o) \
	$(ALIB)(strmbl.o) \
	$(ALIB)(strmnm.o) \
	$(ALIB)(strmst.o) \
	$(ALIB)(strnan.o) \
	$(ALIB)(strnul.o) \
	$(ALIB)(strpsl.o) \
	$(ALIB)(strpst.o) \
	$(ALIB)(strxbl.o) \
	$(ALIB)(stsort.o) \
	$(ALIB)(ststoi.o) \
	$(ALIB)(stuclc.o) \
	$(ALIB)(stunp1.o) \
	$(ALIB)(stunpr.o) \
	$(ALIB)(stutab.o) \
	$(ALIB)(stword.o)
 
GDIR = st

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

