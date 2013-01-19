#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)


COBJS = \
	$(ALIB)(ghadvn.o) \
	$(ALIB)(ghbkrv.o) \
	$(ALIB)(ghfstn.o) \
	$(ALIB)(ghftbl.o) \
	$(ALIB)(ghgclr.o) \
	$(ALIB)(ghrtbl.o) \
	$(ALIB)(ghwtcb.o) \
	$(ALIB)(ghwtce.o) \
	$(ALIB)(ghwtct.o) \
	$(ALIB)(ghwwat.o) \
	$(ALIB)(ghwwbl.o) \
	$(ALIB)(ghwwcl.o) \
	$(ALIB)(ghwwld.o) \
	$(ALIB)(ghwwlg.o) \
	$(ALIB)(ghwwln.o) \
	$(ALIB)(ghwwlr.o) \
	$(ALIB)(ghwwlv.o) \
	$(ALIB)(ghwwpr.o) \
	$(ALIB)(ghwwug.o)

FOBJS = \
	$(ALIB)(gh0bkp.o) \
	$(ALIB)(ghbkat.o) \
	$(ALIB)(ghbkcal.o) \
	$(ALIB)(ghbkgb.o) \
	$(ALIB)(ghbkloc.o) \
	$(ALIB)(ghbkme.o) \
	$(ALIB)(ghbkpl.o) \
	$(ALIB)(ghbkpo.o) \
	$(ALIB)(ghbkpp.o) \
	$(ALIB)(ghbkpt.o) \
	$(ALIB)(ghbksq.o) \
	$(ALIB)(ghbktb.o) \
	$(ALIB)(ghcolr.o) \
	$(ALIB)(ghgbkl.o) \
	$(ALIB)(ghkgar.o) \
	$(ALIB)(ghkgat.o) \
	$(ALIB)(ghkgfl.o) \
	$(ALIB)(ghkgip.o) \
	$(ALIB)(ghkgix.o) \
	$(ALIB)(ghkglb.o) \
	$(ALIB)(ghkglc.o) \
	$(ALIB)(ghkglp.o) \
	$(ALIB)(ghkgmp.o) \
	$(ALIB)(ghkgov.o) \
	$(ALIB)(ghkgst.o) \
	$(ALIB)(ghkgtb.o) \
	$(ALIB)(ghnopb.o) \
	$(ALIB)(ghprpt.o) \
	$(ALIB)(ghpuhd.o) \
	$(ALIB)(ghpsum.o) \
	$(ALIB)(ghpsum09.o) \
	$(ALIB)(ghrdad.o) \
	$(ALIB)(ghrdbd.o) \
	$(ALIB)(ghrdds.o) \
	$(ALIB)(ghrdfv.o) \
	$(ALIB)(ghrdhd.o) \
	$(ALIB)(ghrdpb.o) \
	$(ALIB)(ghrdqd.o) \
	$(ALIB)(ghrdsp.o) \
	$(ALIB)(ghrdtx.o) \
	$(ALIB)(ghrest.o) \
	$(ALIB)(ghrhst.o) \
	$(ALIB)(ghrprb.o) \
	$(ALIB)(ghrscl.o) \
	$(ALIB)(ghsave.o) \
	$(ALIB)(ghsplb.o) \
	$(ALIB)(ghsprb.o) \
	$(ALIB)(ghsprl.o) \
	$(ALIB)(ghsvcl.o) \
	$(ALIB)(ghswdf.o) \
	$(ALIB)(ghswlb.o) \
	$(ALIB)(ghswln.o) \
	$(ALIB)(ghswth.o) \
	$(ALIB)(ghtc2vg.o) \
	$(ALIB)(ghtclb.o) \
	$(ALIB)(ghtctx.o) \
	$(ALIB)(ghtcww.o) \
	$(ALIB)(ghtime.o) \
	$(ALIB)(ghuhst.o) \
	$(ALIB)(ghwcnd.o) \
	$(ALIB)(ghwgrf.o) \
	$(ALIB)(ghwier.o) \
	$(ALIB)(ghwipl.o) \
	$(ALIB)(ghwpnl.o) \
	$(ALIB)(ghwrel.o) \
	$(ALIB)(ghwrng.o) \
	$(ALIB)(ghwtbg.o) \
	$(ALIB)(ghwtbl.o) \
	$(ALIB)(ghwtpl.o) \
	$(ALIB)(ghwwbk.o) \
	$(ALIB)(ghwwcv.o) \
	$(ALIB)(ghwwcz.o) \
	$(ALIB)(ghwwdi.o) \
	$(ALIB)(ghwwdp.o) \
	$(ALIB)(ghwwhi.o) \
	$(ALIB)(ghwwin.o) \
	$(ALIB)(ghwwmv.o) \
	$(ALIB)(ghwwov.o) \
	$(ALIB)(ghwwrm.o) \
	$(ALIB)(ghwwsg.o) \
	$(ALIB)(ghwwsr.o) \
	$(ALIB)(ghwwst.o) \
	$(ALIB)(ghwwtx.o) \
	$(ALIB)(ghwwvc.o)

GDIR = gh

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
	$(LINK.f) test$(GDIR).o -o test$(GDIR) $(GEMLIB) $(APPL) $(SYSLIB) $(GEMLIB)
	$(INSTALL) test$(GDIR) $(OS_BIN)
	$(RM) *.o test$(GDIR)

