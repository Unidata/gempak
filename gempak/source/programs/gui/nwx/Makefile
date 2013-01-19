#
#	Makefile for ntl
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

PROG = nwx

ALIB = $(OS_LIB)/nwx.a

LOBJS = \
	$(ALIB)(nwxg_auto.o) \
	$(ALIB)(nwxp_dchk.o) \
	$(ALIB)(nwxp_dir.o) \
	$(ALIB)(nwxp_draw.o) \
	$(ALIB)(nwxp_dslw.o) \
	$(ALIB)(nwxg_dslw.o) \
	$(ALIB)(nwxp_dttm.o) \
	$(ALIB)(nwxp_fosd.o) \
	$(ALIB)(mapdraw.o) \
	$(ALIB)(mapmark.o) \
	$(ALIB)(nwxp_txtw.o) \
	$(ALIB)(nwxg_txtw.o) \
	$(ALIB)(nwxg_mapw.o) \
	$(ALIB)(nwxp_mapw.o) \
	$(ALIB)(nsfopn.o) \
	$(ALIB)(nwxg_err.o) \
	$(ALIB)(nwxp_tbl.o) \
	$(ALIB)(nwxp_prf.o) \
	$(ALIB)(nwxp_qpf.o) \
	$(ALIB)(nwxp_srchb.o) \
	$(ALIB)(nwxp_srcho.o) \
	$(ALIB)(nwxp_srchw.o) \
	$(ALIB)(nwxp_sstruct.o) \
	$(ALIB)(nwxp_uvi.o) \
	$(ALIB)(nwxp_wbox.o) \
	$(ALIB)(wwaxpt.o) \
	$(ALIB)(nwxp_wwcrnr.o) \
	$(ALIB)(wwctim.o) \
	$(ALIB)(wwdcod.o) \
	$(ALIB)(wwrtim.o) \
	$(ALIB)(wwus40.o) \
	$(ALIB)(wwus9.o) \
	$(ALIB)(wwwnum.o) \
	$(ALIB)(nwxp_idata.o) \
	$(ALIB)(nwxp_pdata.o) \
	$(ALIB)(nwx_wgem.o) \
	$(ALIB)(nwx_wnxm.o) \
	$(ALIB)(nwxg_nwx.o)

LIBINC = \
	$(GPLT_NC) \
	$(NXMLIB) \
	$(GEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(XWP) \
	$(XW) \
	$(PS) \
	$(GN) \
	$(CGEMLIB) \
	$(GPLT) \
	$(GEMLIB) \
	$(NETCDF) \
	$(PNGLIB) \
        $(XMLLIB) \
	$(ZLIB) \
	$(RSLLIB) \
	$(LIBBZ2) \
	$(GPLT)

CFLAGS = $(COPT) $(XWINCDIR) $(MOTIFINC)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG) : $(ALIB) $(PROG).c
	$(COMPILE.c) $@.c
	$(LINK.f) -o $@ $@.o \
		$(NOFORMAIN) \
		$(ALIB) \
		$(LIBINC) \
		$(GUILIBS) $(SYSLIBS) -lm 
#	$(RM) $@.o

$(ALIB): $(LOBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(RM) $?

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) $(PROG) *.o 



install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)
