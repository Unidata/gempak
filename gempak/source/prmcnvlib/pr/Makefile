#
#	Makefile for GEMPAK 6.0
#		Builds gemlib.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(GEMLIB)

FOBJS = \
	$(ALIB)(pralti.o) \
	$(ALIB)(praltm.o) \
	$(ALIB)(praltp.o) \
	$(ALIB)(pramsl.o) \
	$(ALIB)(prawnm.o) \
	$(ALIB)(prcfct.o) \
	$(ALIB)(prclct.o) \
	$(ALIB)(prclcx.o) \
	$(ALIB)(prcldb.o) \
	$(ALIB)(prclhx.o) \
	$(ALIB)(prcloa.o) \
	$(ALIB)(prcmbc.o) \
	$(ALIB)(prcmsl.o) \
	$(ALIB)(prcomh.o) \
	$(ALIB)(prcoml.o) \
	$(ALIB)(prcomm.o) \
	$(ALIB)(prcomt.o) \
	$(ALIB)(prcomx.o) \
	$(ALIB)(prcsyh.o) \
	$(ALIB)(prcsyl.o) \
	$(ALIB)(prcsym.o) \
	$(ALIB)(prcsyt.o) \
	$(ALIB)(prctcc.o) \
	$(ALIB)(prctcf.o) \
	$(ALIB)(prd100.o) \
	$(ALIB)(prdden.o) \
	$(ALIB)(prddep.o) \
	$(ALIB)(prdmax.o) \
	$(ALIB)(prdmin.o) \
	$(ALIB)(prdrct.o) \
	$(ALIB)(prdwdp.o) \
	$(ALIB)(prdwpt.o) \
	$(ALIB)(prfosb.o) \
	$(ALIB)(prhcdm.o) \
	$(ALIB)(prheat.o) \
	$(ALIB)(prhgfm.o) \
	$(ALIB)(prhgfs.o) \
	$(ALIB)(prhgkm.o) \
	$(ALIB)(prhgmd.o) \
	$(ALIB)(prhgmf.o) \
	$(ALIB)(prhgmk.o) \
	$(ALIB)(prhgnm.o) \
	$(ALIB)(prhgsf.o) \
	$(ALIB)(prhmtr.o) \
	$(ALIB)(prigro.o) \
	$(ALIB)(prinmm.o) \
	$(ALIB)(prknmh.o) \
	$(ALIB)(prknms.o) \
	$(ALIB)(prlati.o) \
	$(ALIB)(prlhvp.o) \
	$(ALIB)(prloni.o) \
	$(ALIB)(prltmp.o) \
	$(ALIB)(prm100.o) \
	$(ALIB)(prmhgt.o) \
	$(ALIB)(prmhkn.o) \
	$(ALIB)(prmixr.o) \
	$(ALIB)(prmmin.o) \
	$(ALIB)(prmobs.o) \
	$(ALIB)(prmskn.o) \
	$(ALIB)(prnsym.o) \
	$(ALIB)(prp03c.o) \
	$(ALIB)(prp03d.o) \
	$(ALIB)(prpalt.o) \
	$(ALIB)(prpany.o) \
	$(ALIB)(prpkdd.o) \
	$(ALIB)(prpkss.o) \
	$(ALIB)(prplcl.o) \
	$(ALIB)(prpmsl.o) \
	$(ALIB)(prpmst.o) \
	$(ALIB)(prpr24.o) \
	$(ALIB)(prpr6x.o) \
	$(ALIB)(prpres.o) \
	$(ALIB)(prpspd.o) \
	$(ALIB)(prptsy.o) \
	$(ALIB)(prpwao.o) \
	$(ALIB)(prquot.o) \
	$(ALIB)(prrelh.o) \
	$(ALIB)(prreli.o) \
	$(ALIB)(prrhdp.o) \
	$(ALIB)(prrzll.o) \
	$(ALIB)(prsali.o) \
	$(ALIB)(prsalt.o) \
	$(ALIB)(prsclh.o) \
	$(ALIB)(prskyx.o) \
	$(ALIB)(prsped.o) \
	$(ALIB)(prstdz.o) \
	$(ALIB)(prthta.o) \
	$(ALIB)(prthte.o) \
	$(ALIB)(prthwc.o) \
	$(ALIB)(prtlcl.o) \
	$(ALIB)(prtmcf.o) \
	$(ALIB)(prtmck.o) \
	$(ALIB)(prtmfc.o) \
	$(ALIB)(prtmfk.o) \
	$(ALIB)(prtmkc.o) \
	$(ALIB)(prtmkf.o) \
	$(ALIB)(prtmpk.o) \
	$(ALIB)(prtmst.o) \
	$(ALIB)(prtmwb.o) \
	$(ALIB)(prtpfr.o) \
	$(ALIB)(prtpwn.o) \
	$(ALIB)(prtvrk.o) \
	$(ALIB)(pruwnd.o) \
	$(ALIB)(prvapi.o) \
	$(ALIB)(prvapr.o) \
	$(ALIB)(prvskn.o) \
	$(ALIB)(prvwnd.o) \
	$(ALIB)(prwccv.o) \
	$(ALIB)(prwceq.o) \
	$(ALIB)(prwcfr.o) \
	$(ALIB)(prwcht.o) \
	$(ALIB)(prwcmp.o) \
	$(ALIB)(prwcms.o) \
	$(ALIB)(prwind.o) \
	$(ALIB)(prwmao.o) \
	$(ALIB)(prwnml.o) \
	$(ALIB)(prwphf.o) \
	$(ALIB)(prwphm.o) \
	$(ALIB)(prwtnd.o) \
	$(ALIB)(prwvdd.o) \
	$(ALIB)(prwvis.o) \
	$(ALIB)(prwvph.o) \
	$(ALIB)(prwvsw.o) \
	$(ALIB)(prwxvf.o) \
	$(ALIB)(prxvfr.o) \
	$(ALIB)(przalt.o)
 
GDIR = pr

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

