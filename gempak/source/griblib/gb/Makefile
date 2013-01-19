#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

COBJS = \
	$(ALIB)(gb22gem.o) \
	$(ALIB)(gb2clos.o) \
	$(ALIB)(gb2ctim.o) \
	$(ALIB)(gb2diag.o) \
	$(ALIB)(gb2ens.o) \
	$(ALIB)(gb2fcsm.o) \
	$(ALIB)(gb2ftim.o) \
	$(ALIB)(gb2gaus.o) \
	$(ALIB)(gb2gdsnav.o) \
	$(ALIB)(gb2gdtlamb.o) \
	$(ALIB)(gb2gdtltln.o) \
	$(ALIB)(gb2gdtmerc.o) \
	$(ALIB)(gb2gdtpstr.o) \
	$(ALIB)(gb2gmis.o) \
	$(ALIB)(gb2grid.o) \
	$(ALIB)(gb2gtcntr.o) \
	$(ALIB)(gb2gtlcllvltbl.o) \
	$(ALIB)(gb2gtlclvartbl.o) \
	$(ALIB)(gb2gtwmolvltbl.o) \
	$(ALIB)(gb2gtwmovartbl.o) \
	$(ALIB)(gb2lamb.o) \
	$(ALIB)(gb2level2g2.o) \
	$(ALIB)(gb2ltln.o) \
	$(ALIB)(gb2merc.o) \
	$(ALIB)(gb2navgdt.o) \
	$(ALIB)(gb2next.o) \
	$(ALIB)(gb2open.o) \
	$(ALIB)(gb2ornt.o) \
	$(ALIB)(gb2param.o) \
	$(ALIB)(gb2param2g2.o) \
	$(ALIB)(gb2polr.o) \
	$(ALIB)(gb2prob.o) \
	$(ALIB)(gb2proc.o) \
	$(ALIB)(gb2qlin.o) \
	$(ALIB)(gb2read.o) \
	$(ALIB)(gb2setftime.o) \
	$(ALIB)(gb2sklvl.o) \
	$(ALIB)(gb2skparam.o) \
	$(ALIB)(gb2skvar.o) \
	$(ALIB)(gb2skvcord.o) \
	$(ALIB)(gb2ugem.o) \
	$(ALIB)(gb2vcrd.o) \
	$(ALIB)(gb2vlev.o) \
	$(ALIB)(gb2wgem.o) \
	$(ALIB)(gbbds.o) \
	$(ALIB)(gbbms.o) \
	$(ALIB)(gbbtoi.o) \
	$(ALIB)(gbclos.o) \
	$(ALIB)(gbdiag.o) \
	$(ALIB)(gbecmwfclu.o) \
	$(ALIB)(gbecmwfcpc.o) \
	$(ALIB)(gbends.o) \
	$(ALIB)(gbensemble.o) \
	$(ALIB)(gbftim.o) \
	$(ALIB)(gbgaus.o) \
	$(ALIB)(gbgbdh.o) \
	$(ALIB)(gbgds.o) \
	$(ALIB)(gbges.o) \
	$(ALIB)(gbggds.o) \
	$(ALIB)(gbgids.o) \
	$(ALIB)(gbgmtx.o) \
	$(ALIB)(gbgpbd.o) \
	$(ALIB)(gbgpds.o) \
	$(ALIB)(gbgsec.o) \
	$(ALIB)(gbgspk.o) \
	$(ALIB)(gbgubd.o) \
	$(ALIB)(gbids.o) \
	$(ALIB)(gblamb.o) \
	$(ALIB)(gbltln.o) \
	$(ALIB)(gbmerc.o) \
	$(ALIB)(gbnext.o) \
	$(ALIB)(gbopen.o) \
	$(ALIB)(gbpds.o) \
	$(ALIB)(gbpolr.o) \
	$(ALIB)(gbread.o) \
	$(ALIB)(gbrindx.o) \
	$(ALIB)(gbscan.o) \
	$(ALIB)(gbscpk.o) \
	$(ALIB)(gbsphr.o) \
	$(ALIB)(gbsspk.o) \
	$(ALIB)(gbstage.o) \
	$(ALIB)(gbtdlens.o) \
	$(ALIB)(gbunpk.o) \
	$(ALIB)(gbvlev.o)
 
GDIR = gb

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

