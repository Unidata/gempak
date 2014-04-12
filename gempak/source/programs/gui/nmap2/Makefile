#
#	Makefile for nmap
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = nmap2

ALIB = $(OS_LIB)/nmap2.a

NMAP_OBJS = \
	$(ALIB)(ncwcreate.o) \
	$(ALIB)(ncwsproj.o) \
	$(ALIB)(nmap_aodtw.o) \
	$(ALIB)(nmap_aodtw64.o) \
	$(ALIB)(nmap_aodtw72.o) \
	$(ALIB)(nmap_auto.o) \
	$(ALIB)(nmap_cldhgtw.o) \
	$(ALIB)(nmap_cmdln.o) \
	$(ALIB)(nmap_cursorw.o) \
	$(ALIB)(nmap_dataw.o) \
	$(ALIB)(nmap_dslw.o) \
	$(ALIB)(nmap_dsp.o) \
	$(ALIB)(nmap_dttm.o) \
	$(ALIB)(nmap_dttmw.o) \
	$(ALIB)(nmap_gmpk.o) \
	$(ALIB)(nmap_grd.o) \
	$(ALIB)(nmap_gtbl.o) \
	$(ALIB)(nmap_image.o) \
	$(ALIB)(nmap_locfmtw.o) \
	$(ALIB)(nmap_logo.o) \
	$(ALIB)(nmap_loop.o) \
	$(ALIB)(nmap_loopw.o) \
	$(ALIB)(nmap_mainw.o) \
	$(ALIB)(nmap_mapset.o) \
	$(ALIB)(nmap_mapw.o) \
	$(ALIB)(nmap_mbotw.o) \
	$(ALIB)(nmap_mbtnw.o) \
	$(ALIB)(nmap_mcanvw.o) \
	$(ALIB)(nmap_mmenuw.o) \
	$(ALIB)(nmap_mpcstw.o) \
	$(ALIB)(nmap_msc.o) \
	$(ALIB)(nmap_obs.o) \
	$(ALIB)(nmap_pgactv.o) \
	$(ALIB)(nmap_pgaddpt.o) \
	$(ALIB)(nmap_pgairmet.o) \
	$(ALIB)(nmap_pgairmetp.o) \
	$(ALIB)(nmap_pgasel.o) \
	$(ALIB)(nmap_pgccfp.o) \
	$(ALIB)(nmap_pgccfw.o) \
	$(ALIB)(nmap_pgcirc.o) \
	$(ALIB)(nmap_pgconn.o) \
	$(ALIB)(nmap_pgcycle.o) \
	$(ALIB)(nmap_pgdel.o) \
	$(ALIB)(nmap_pgdelobj.o) \
	$(ALIB)(nmap_pgdelpt.o) \
	$(ALIB)(nmap_pgdist.o) \
	$(ALIB)(nmap_pgedit.o) \
	$(ALIB)(nmap_pgevt.o) \
	$(ALIB)(nmap_pgextrap.o) \
	$(ALIB)(nmap_pgfilterw.o) \
	$(ALIB)(nmap_pgfilw.o) \
	$(ALIB)(nmap_pgfrom.o) \
	$(ALIB)(nmap_pgfrtw.o) \
	$(ALIB)(nmap_pggfaw.o) \
	$(ALIB)(nmap_pggfawp.o) \
	$(ALIB)(nmap_pggfmt.o) \
	$(ALIB)(nmap_pggrpch.o) \
	$(ALIB)(nmap_pggrpw.o) \
	$(ALIB)(nmap_pggst.o) \
	$(ALIB)(nmap_pghdlb.o) \
	$(ALIB)(nmap_pginterp.o) \
	$(ALIB)(nmap_pgjet.o) \
	$(ALIB)(nmap_pglabel.o) \
	$(ALIB)(nmap_pglayer.o) \
	$(ALIB)(nmap_pglayred.o) \
	$(ALIB)(nmap_pglayrnm.o) \
	$(ALIB)(nmap_pglayrw.o) \
	$(ALIB)(nmap_pglayrxt.o) \
	$(ALIB)(nmap_pgline.o) \
	$(ALIB)(nmap_pglist.o) \
	$(ALIB)(nmap_pglpfw.o) \
	$(ALIB)(nmap_pgmdfy.o) \
	$(ALIB)(nmap_pgmodv.o) \
	$(ALIB)(nmap_pgmsel.o) \
	$(ALIB)(nmap_pgmvcp.o) \
	$(ALIB)(nmap_pgnew.o) \
	$(ALIB)(nmap_pgnumb.o) \
	$(ALIB)(nmap_pgobj.o) \
	$(ALIB)(nmap_pgofmt.o) \
	$(ALIB)(nmap_pgolk.o) \
	$(ALIB)(nmap_pgpalw.o) \
	$(ALIB)(nmap_pgpdel.o) \
	$(ALIB)(nmap_pgprd.o) \
	$(ALIB)(nmap_pgqpf.o) \
	$(ALIB)(nmap_pgrad.o) \
	$(ALIB)(nmap_pgrot.o) \
	$(ALIB)(nmap_pgsfp.o) \
	$(ALIB)(nmap_pgsigw.o) \
	$(ALIB)(nmap_pgsmear.o) \
	$(ALIB)(nmap_pgsymb.o) \
	$(ALIB)(nmap_pgtca.o) \
	$(ALIB)(nmap_pgtrk.o) \
	$(ALIB)(nmap_pgtrkw.o) \
	$(ALIB)(nmap_pgtxt.o) \
	$(ALIB)(nmap_pgundo.o) \
	$(ALIB)(nmap_pgutls.o) \
	$(ALIB)(nmap_pgvacw.o) \
	$(ALIB)(nmap_pgvgf.o) \
	$(ALIB)(nmap_pgvolw.o) \
	$(ALIB)(nmap_pgwatch.o) \
	$(ALIB)(nmap_pgwbxw.o) \
	$(ALIB)(nmap_pgwcnsl.o) \
	$(ALIB)(nmap_pgwfmt.o) \
	$(ALIB)(nmap_pgwlst.o) \
	$(ALIB)(nmap_pgwndw.o) \
	$(ALIB)(nmap_pgwpts.o) \
	$(ALIB)(nmap_pgwsmw.o) \
	$(ALIB)(nmap_pgwxd.o) \
	$(ALIB)(nmap_pgxrain.o) \
	$(ALIB)(nmap_roamw.o) \
	$(ALIB)(nmap_rsrc.o) \
	$(ALIB)(nmap_seekw.o) \
	$(ALIB)(nmap_spfw.o) \
	$(ALIB)(nmap_stbl.o) \
	$(ALIB)(nmap_stnm.o) \
	$(ALIB)(nmap_stnmw.o) \
	$(ALIB)(nmap_tmln.o) \
	$(ALIB)(nmap_vgf.o) \
	$(ALIB)(nmap_vtbl.o) \
	$(ALIB)(nmap_zoomw.o)  \
	$(ALIB)(image_props.o) \
	$(ALIB)(imgpix.o)

DEVLIBS = \
	$(OS_LIB)/xwp.a \
	$(OS_LIB)/xw.a \
	$(PS) \
	$(GN)

LIBINC = \
	$(ALIB) \
	$(GEMLIB)\
	$(GPLT_NC) \
	$(OS_LIB)/nmaplib.a \
	$(ALIB) \
	$(NXMLIB) \
	$(OS_LIB)/sfmap.a \
	$(OS_LIB)/snmap.a \
	$(OS_LIB)/gdplot2.a \
	$(OS_LIB)/gdmap.a \
	$(OS_LIB)/gdstream.a \
	$(OS_LIB)/grphgd.a \
	$(OS_LIB)/gdcfil.a \
	$(OS_LIB)/oabsfc.a \
	$(OS_LIB)/libAODT_v64.a $(OS_LIB)/libAODT_v72.a \
	$(GEMLIB)\
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(DEVLIBS) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(LIBGPC) \
	$(NETCDF) \
	$(LIBJASPER) \
	$(PNGLIB) \
	$(OS_LIB)/libmel_bufr.a \
	$(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(XMLDEP) \
	$(CGEMLIB) \
	$(GEMLIB)

.PRECIOUS: $(ALIB) 

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC) -I$(NAWIPS)/extlibs/AODT

all : $(PROG)

$(PROG) : $(ALIB) nmap2.c
	$(COMPILE.c) nmap2.c
	$(LINK.f) -o $@ $(NOFORMAIN) \
		nmap2.o \
		$(LIBINC) \
		$(GUILIBS) $(ICONVLIB) -lm -lrt

$(ALIB): $(NMAP_OBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
