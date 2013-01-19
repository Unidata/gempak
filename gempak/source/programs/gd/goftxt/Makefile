#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

COBJS =

LOBJS = \
	$(ALIB)(g2t_a.o) \
	$(ALIB)(g2t_a2raeb.o) \
	$(ALIB)(g2t_a2raec.o) \
	$(ALIB)(g2t_a2rbeb.o) \
	$(ALIB)(g2t_a2rbec.o) \
	$(ALIB)(g2t_a2rceb.o) \
	$(ALIB)(g2t_a2rcec.o) \
	$(ALIB)(g2t_abc.o) \
	$(ALIB)(g2t_appex.o) \
	$(ALIB)(g2t_apptxt.o) \
	$(ALIB)(g2t_awstxt.o) \
	$(ALIB)(g2t_bnd.o) \
	$(ALIB)(g2t_case8.o) \
	$(ALIB)(g2t_center.o) \
	$(ALIB)(g2t_check.o) \
	$(ALIB)(g2t_clos.o) \
	$(ALIB)(g2t_compass.o) \
	$(ALIB)(g2t_dayw.o) \
	$(ALIB)(g2t_driv.o) \
	$(ALIB)(g2t_dsdt.o) \
	$(ALIB)(g2t_dwdt.o) \
	$(ALIB)(g2t_except.o) \
	$(ALIB)(g2t_fill.o) \
	$(ALIB)(g2t_gap.o) \
	$(ALIB)(g2t_get.o) \
	$(ALIB)(g2t_gszone.o) \
	$(ALIB)(g2t_gtext.o) \
	$(ALIB)(g2t_headln.o) \
	$(ALIB)(g2t_hist.o) \
	$(ALIB)(g2t_init.o) \
	$(ALIB)(g2t_miss.o) \
	$(ALIB)(g2t_mwdir.o) \
	$(ALIB)(g2t_mode.o) \
	$(ALIB)(g2t_open.o) \
	$(ALIB)(g2t_opnf.o) \
	$(ALIB)(g2t_outlook.o) \
	$(ALIB)(g2t_output.o) \
	$(ALIB)(g2t_raeb.o) \
	$(ALIB)(g2t_range.o) \
	$(ALIB)(g2t_rangx.o) \
	$(ALIB)(g2t_rdparm.o) \
	$(ALIB)(g2t_rdzone.o) \
	$(ALIB)(g2t_rules.o) \
	$(ALIB)(g2t_savw.o) \
	$(ALIB)(g2t_sort.o) \
	$(ALIB)(g2t_squad.o) \
	$(ALIB)(g2t_stat.o) \
	$(ALIB)(g2t_synop.o) \
	$(ALIB)(g2t_trim.o) \
	$(ALIB)(g2t_trim2.o) \
	$(ALIB)(g2t_wdir.o) \
	$(ALIB)(g2t_wdir1.o) \
	$(ALIB)(g2t_wdir2.o) \
	$(ALIB)(g2t_wrn1.o) \
	$(ALIB)(g2t_wrn2.o) \
	$(ALIB)(g2t_wrnx.o) \
	$(ALIB)(g2t_wspread.o)

ALIB = $(OS_LIB)/goftxt.a
 
PROG = goftxt
PROG.f = goftxt.f
 
LIBINC = \
	$(OS_LIB)/gddiag.a \
	$(OS_LIB)/gdcfil.a \
	$(GPLT_NC) \
	$(GEMLIB) \
	$(CGEMLIB) \
	$(GPLT) \
	$(DEVICE) \
	$(GN) \
	$(CGEMLIB) \
	$(GEMLIB) \
	$(LIBJASPER) \
	$(PNGLIB) \
        $(XMLLIB) \
        $(ZLIB) \
	$(CGEMLIB)

.PRECIOUS: $(ALIB)

all : $(ALIB) $(PROG)

$(PROG): $(ALIB) $(LIBINC) $(PROG.f)
	$(LINK.f) $(PROG.f) $(ALIB) $(LIBINC) $(SYSLIBS) -lm -o $@


programs_gf:
	@echo "Not a graphic program, no _gf version needed"

programs_nc:
	@echo "Not a graphic program, no _nc version needed"

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o $(PROG)

install: $(PROG)
	$(INSTALL) $(PROG) $(OS_BIN)

