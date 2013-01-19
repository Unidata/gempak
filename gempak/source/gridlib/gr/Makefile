#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(gracol.o) \
	$(ALIB)(gralgn.o) \
	$(ALIB)(graxlv.o) \
	$(ALIB)(grclvl.o) \
	$(ALIB)(grcmpv.o) \
	$(ALIB)(grcnav.o) \
	$(ALIB)(grcval.o) \
	$(ALIB)(grdorg.o) \
	$(ALIB)(grfixa.o) \
	$(ALIB)(grftim.o) \
	$(ALIB)(grgalm.o) \
	$(ALIB)(grgtim.o) \
	$(ALIB)(grintp.o) \
	$(ALIB)(grlabl.o) \
	$(ALIB)(grlevl.o) \
	$(ALIB)(grlist.o) \
	$(ALIB)(grltln.o) \
	$(ALIB)(grmban.o) \
	$(ALIB)(grmbn2.o) \
	$(ALIB)(grmnav.o) \
	$(ALIB)(grmttl.o) \
	$(ALIB)(grndcp.o) \
	$(ALIB)(grnlev.o) \
	$(ALIB)(gropen.o) \
	$(ALIB)(grpack.o) \
	$(ALIB)(grplin.o) \
	$(ALIB)(grploc.o) \
	$(ALIB)(grrarg.o) \
	$(ALIB)(grrban.o) \
	$(ALIB)(grrnav.o) \
	$(ALIB)(grscal.o) \
	$(ALIB)(grsetr.o) \
	$(ALIB)(grshrt.o) \
	$(ALIB)(grsnav.o) \
	$(ALIB)(grsscl.o) \
	$(ALIB)(grstat.o) \
	$(ALIB)(grsub2.o) \
	$(ALIB)(grsuba.o) \
	$(ALIB)(grsubx.o) \
	$(ALIB)(grtitl.o) \
	$(ALIB)(grtlst.o) \
	$(ALIB)(grtmfl.o) \
	$(ALIB)(grvnav.o) \
	$(ALIB)(grvscl.o) \
	$(ALIB)(grwgb2.o) \
	$(ALIB)(grwnmc.o) \
	$(ALIB)(grwtrm.o)
 
GDIR = gr

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(CGEMLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

