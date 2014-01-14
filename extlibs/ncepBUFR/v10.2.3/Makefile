#
#	Makefile for GEMPAK 6.0
#		Builds libaodt.a library archive
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(OS_LIB)/libncepBUFR.a

LOBJS = \
	$(ALIB)(bort_exit.o) \
	$(ALIB)(ccbfl.o) \
	$(ALIB)(cmpia.o) \
	$(ALIB)(cobfl.o) \
	$(ALIB)(crbmg.o) \
	$(ALIB)(cread.o) \
	$(ALIB)(cwbmg.o) \
	$(ALIB)(icvidx.o) \
	$(ALIB)(nummtb.o) \
	$(ALIB)(rbytes.o) \
	$(ALIB)(restd.o) \
	$(ALIB)(stseq.o) \
	$(ALIB)(wrdesc.o) \
	$(ALIB)(adn30.o) \
	$(ALIB)(atrcpt.o) \
	$(ALIB)(bfrini.o) \
	$(ALIB)(blocks.o) \
	$(ALIB)(bort2.o) \
	$(ALIB)(bort.o) \
	$(ALIB)(bvers.o) \
	$(ALIB)(cadn30.o) \
	$(ALIB)(capit.o) \
	$(ALIB)(chekstab.o) \
	$(ALIB)(chrtrna.o) \
	$(ALIB)(chrtrn.o) \
	$(ALIB)(cktaba.o) \
	$(ALIB)(closbf.o) \
	$(ALIB)(closmg.o) \
	$(ALIB)(cmpmsg.o) \
	$(ALIB)(cmsgini.o) \
	$(ALIB)(cnved4.o) \
	$(ALIB)(conwin.o) \
	$(ALIB)(copybf.o) \
	$(ALIB)(copymg.o) \
	$(ALIB)(copysb.o) \
	$(ALIB)(cpbfdx.o) \
	$(ALIB)(cpdxmm.o) \
	$(ALIB)(cpymem.o) \
	$(ALIB)(cpyupd.o) \
	$(ALIB)(datebf.o) \
	$(ALIB)(datelen.o) \
	$(ALIB)(digit.o) \
	$(ALIB)(drfini.o) \
	$(ALIB)(drstpl.o) \
	$(ALIB)(dumpbf.o) \
	$(ALIB)(dxdump.o) \
	$(ALIB)(dxinit.o) \
	$(ALIB)(dxmini.o) \
	$(ALIB)(elemdx.o) \
	$(ALIB)(errwrt.o) \
	$(ALIB)(getabdb.o) \
	$(ALIB)(getbmiss.o) \
	$(ALIB)(getlens.o) \
	$(ALIB)(getntbe.o) \
	$(ALIB)(gets1loc.o) \
	$(ALIB)(gettagpr.o) \
	$(ALIB)(gettbh.o) \
	$(ALIB)(getvalnb.o) \
	$(ALIB)(getwin.o) \
	$(ALIB)(i4dy.o) \
	$(ALIB)(ibfms.o) \
	$(ALIB)(icbfms.o) \
	$(ALIB)(ichkstr.o) \
	$(ALIB)(icmpdx.o) \
	$(ALIB)(icopysb.o) \
	$(ALIB)(idn30.o) \
	$(ALIB)(idxmsg.o) \
	$(ALIB)(ifbget.o) \
	$(ALIB)(ifxy.o) \
	$(ALIB)(igetdate.o) \
	$(ALIB)(igetfxy.o) \
	$(ALIB)(igetntbi.o) \
	$(ALIB)(igetntbl.o) \
	$(ALIB)(igetsc.o) \
	$(ALIB)(igettdi.o) \
	$(ALIB)(inctab.o) \
	$(ALIB)(invcon.o) \
	$(ALIB)(invmrg.o) \
	$(ALIB)(invtag.o) \
	$(ALIB)(invwin.o) \
	$(ALIB)(iok2cpy.o) \
	$(ALIB)(ipkm.o) \
	$(ALIB)(ipks.o) \
	$(ALIB)(ireadmg.o) \
	$(ALIB)(ireadmm.o) \
	$(ALIB)(ireadns.o) \
	$(ALIB)(ireadsb.o) \
	$(ALIB)(ishrdx.o) \
	$(ALIB)(isize.o) \
	$(ALIB)(istdesc.o) \
	$(ALIB)(iupb.o) \
	$(ALIB)(iupbs01.o) \
	$(ALIB)(iupbs3.o) \
	$(ALIB)(iupm.o) \
	$(ALIB)(iupvs01.o) \
	$(ALIB)(jstchr.o) \
	$(ALIB)(jstnum.o) \
	$(ALIB)(lcmgdf.o) \
	$(ALIB)(lmsg.o) \
	$(ALIB)(lstjpb.o) \
	$(ALIB)(makestab.o) \
	$(ALIB)(maxout.o) \
	$(ALIB)(mesgbc.o) \
	$(ALIB)(mesgbf.o) \
	$(ALIB)(minimg.o) \
	$(ALIB)(mova2i.o) \
	$(ALIB)(mrginv.o) \
	$(ALIB)(msgfull.o) \
	$(ALIB)(msgini.o) \
	$(ALIB)(msgupd.o) \
	$(ALIB)(msgwrt.o) \
	$(ALIB)(mtinfo.o) \
	$(ALIB)(mvb.o) \
	$(ALIB)(nemock.o) \
	$(ALIB)(nemtab.o) \
	$(ALIB)(nemtba.o) \
	$(ALIB)(nemtbax.o) \
	$(ALIB)(nemtbb.o) \
	$(ALIB)(nemtbd.o) \
	$(ALIB)(nenubd.o) \
	$(ALIB)(nevn.o) \
	$(ALIB)(newwin.o) \
	$(ALIB)(nmsub.o) \
	$(ALIB)(nmwrd.o) \
	$(ALIB)(numbck.o) \
	$(ALIB)(numtab.o) \
	$(ALIB)(numtbd.o) \
	$(ALIB)(nvnwin.o) \
	$(ALIB)(nwords.o) \
	$(ALIB)(nxtwin.o) \
	$(ALIB)(openbf.o) \
	$(ALIB)(openbt.o) \
	$(ALIB)(openmb.o) \
	$(ALIB)(openmg.o) \
	$(ALIB)(pad.o) \
	$(ALIB)(padmsg.o) \
	$(ALIB)(parstr.o) \
	$(ALIB)(parusr.o) \
	$(ALIB)(parutg.o) \
	$(ALIB)(pkb.o) \
	$(ALIB)(pkbs1.o) \
	$(ALIB)(pkc.o) \
	$(ALIB)(pkftbv.o) \
	$(ALIB)(pktdd.o) \
	$(ALIB)(pkvs01.o) \
	$(ALIB)(posapx.o) \
	$(ALIB)(rcstpl.o) \
	$(ALIB)(rdbfdx.o) \
	$(ALIB)(rdcmps.o) \
	$(ALIB)(rdmemm.o) \
	$(ALIB)(rdmems.o) \
	$(ALIB)(rdmgsb.o) \
	$(ALIB)(rdmsgb.o) \
	$(ALIB)(rdmsgw.o) \
	$(ALIB)(rdmtbb.o) \
	$(ALIB)(rdmtbd.o) \
	$(ALIB)(rdtree.o) \
	$(ALIB)(rdusdx.o) \
	$(ALIB)(readdx.o) \
	$(ALIB)(readerme.o) \
	$(ALIB)(readlc.o) \
	$(ALIB)(readmg.o) \
	$(ALIB)(readmm.o) \
	$(ALIB)(readmt.o) \
	$(ALIB)(readns.o) \
	$(ALIB)(reads3.o) \
	$(ALIB)(readsb.o) \
	$(ALIB)(rewnbf.o) \
	$(ALIB)(rjust.o) \
	$(ALIB)(rsvfvm.o) \
	$(ALIB)(rtrcpt.o) \
	$(ALIB)(seqsdx.o) \
	$(ALIB)(setblock.o) \
	$(ALIB)(setbmiss.o) \
	$(ALIB)(sntbbe.o) \
	$(ALIB)(sntbde.o) \
	$(ALIB)(status.o) \
	$(ALIB)(stbfdx.o) \
	$(ALIB)(stdmsg.o) \
	$(ALIB)(stndrd.o) \
	$(ALIB)(stntbia.o) \
	$(ALIB)(stntbi.o) \
	$(ALIB)(strcln.o) \
	$(ALIB)(strcpt.o) \
	$(ALIB)(string.o) \
	$(ALIB)(strlng.o) \
	$(ALIB)(strnum.o) \
	$(ALIB)(strsuc.o) \
	$(ALIB)(tabent.o) \
	$(ALIB)(tabsub.o) \
	$(ALIB)(trybump.o) \
	$(ALIB)(ufbcnt.o) \
	$(ALIB)(ufbcpy.o) \
	$(ALIB)(ufbcup.o) \
	$(ALIB)(ufbdmp.o) \
	$(ALIB)(ufbevn.o) \
	$(ALIB)(ufbget.o) \
	$(ALIB)(ufbin3.o) \
	$(ALIB)(ufbint.o) \
	$(ALIB)(ufbinx.o) \
	$(ALIB)(ufbmem.o) \
	$(ALIB)(ufbmex.o) \
	$(ALIB)(ufbmms.o) \
	$(ALIB)(ufbmns.o) \
	$(ALIB)(ufbovr.o) \
	$(ALIB)(ufbpos.o) \
	$(ALIB)(ufbqcd.o) \
	$(ALIB)(ufbqcp.o) \
	$(ALIB)(ufbrep.o) \
	$(ALIB)(ufbrms.o) \
	$(ALIB)(ufbrp.o) \
	$(ALIB)(ufbrw.o) \
	$(ALIB)(ufbseq.o) \
	$(ALIB)(ufbsp.o) \
	$(ALIB)(ufbstp.o) \
	$(ALIB)(ufbtab.o) \
	$(ALIB)(ufbtam.o) \
	$(ALIB)(ufdump.o) \
	$(ALIB)(upbb.o) \
	$(ALIB)(upb.o) \
	$(ALIB)(upc.o) \
	$(ALIB)(upds3.o) \
	$(ALIB)(upftbv.o) \
	$(ALIB)(ups.o) \
	$(ALIB)(uptdd.o) \
	$(ALIB)(usrtpl.o) \
	$(ALIB)(valx.o) \
	$(ALIB)(wrcmps.o) \
	$(ALIB)(wrdxtb.o) \
	$(ALIB)(writcp.o) \
	$(ALIB)(writdx.o) \
	$(ALIB)(writlc.o) \
	$(ALIB)(writsa.o) \
	$(ALIB)(writsb.o) \
	$(ALIB)(wrtree.o) \
	$(ALIB)(wtstat.o) 

.PRECIOUS: $(ALIB)

all :  bufrlib.prm $(ALIB)

$(ALIB): $(LOBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o bufrlib.prm

.c.a :
	$(CC) $(CFLAGS) -c $<
.f.a :
	$(FC) $(FFLAGS) -c $<

install:

clean:
	$(RM) *.o 

programs_nc:

programs_gf:

