#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(pccing.o) \
	$(ALIB)(pcckpm.o) \
	$(ALIB)(pcclcd.o) \
	$(ALIB)(pcclyr.o) \
	$(ALIB)(pccmdt.o) \
	$(ALIB)(pccmlv.o) \
	$(ALIB)(pccmst.o) \
	$(ALIB)(pccmvr.o) \
	$(ALIB)(pccmvs.o) \
	$(ALIB)(pccomp.o) \
	$(ALIB)(pccscd.o) \
	$(ALIB)(pccstb.o) \
	$(ALIB)(pcdfls.o) \
	$(ALIB)(pcdflv.o) \
	$(ALIB)(pcdfst.o) \
	$(ALIB)(pcdhgt.o) \
	$(ALIB)(pcdpth.o) \
	$(ALIB)(pcflvl.o) \
	$(ALIB)(pcfndl.o) \
	$(ALIB)(pcftop.o) \
	$(ALIB)(pcfunc.o) \
	$(ALIB)(pcgcnd.o) \
	$(ALIB)(pcglev.o) \
	$(ALIB)(pcinid.o) \
	$(ALIB)(pcinit.o) \
	$(ALIB)(pcinth.o) \
	$(ALIB)(pcintp.o) \
	$(ALIB)(pcintt.o) \
	$(ALIB)(pclyrd.o) \
	$(ALIB)(pcmand.o) \
	$(ALIB)(pcmeth.o) \
	$(ALIB)(pcmhgt.o) \
	$(ALIB)(pcmvrt.o) \
	$(ALIB)(pcphgt.o) \
	$(ALIB)(pcpsym.o) \
	$(ALIB)(pcpval.o) \
	$(ALIB)(pcpwtr.o) \
	$(ALIB)(pcsfcz.o) \
	$(ALIB)(pcsing.o) \
	$(ALIB)(pcsint.o) \
	$(ALIB)(pcslcd.o) \
	$(ALIB)(pcslyr.o) \
	$(ALIB)(pcsscd.o) \
	$(ALIB)(pcsstn.o) \
	$(ALIB)(pcstim.o) \
	$(ALIB)(pcsvrt.o)
 
GDIR = pc

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

