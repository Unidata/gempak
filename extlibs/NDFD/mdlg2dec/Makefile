#
#	Makefile for GEMPAK 6.0
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
#################################
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(unpk_swap.o)

LOBJS = \
	$(ALIB)(boundary.o) \
	$(ALIB)(endomess.o) \
	$(ALIB)(ierx.o) \
	$(ALIB)(locpos.o) \
	$(ALIB)(rdieee.o) \
	$(ALIB)(timpr.o) \
	$(ALIB)(unboustro.o) \
	$(ALIB)(unearth.o) \
	$(ALIB)(unpk_azimuth.o) \
	$(ALIB)(unpk_cmplx.o) \
	$(ALIB)(unpk_cylinder.o) \
	$(ALIB)(unpk_endian.o) \
	$(ALIB)(unpk_equator.o) \
	$(ALIB)(unpk_grib2.o) \
	$(ALIB)(unpk_lambert.o) \
	$(ALIB)(unpk_mercator.o) \
	$(ALIB)(unpk_orthographic.o) \
	$(ALIB)(unpk_polster.o) \
	$(ALIB)(unpk_refer.o) \
	$(ALIB)(unpk_sect0.o) \
	$(ALIB)(unpk_sect1.o) \
	$(ALIB)(unpk_sect2.o) \
	$(ALIB)(unpk_sect3.o) \
	$(ALIB)(unpk_sect4.o) \
	$(ALIB)(unpk_sect5.o) \
	$(ALIB)(unpk_sect6.o) \
	$(ALIB)(unpk_sect7.o) \
	$(ALIB)(unpk_smple.o) \
	$(ALIB)(unpk_temp40.o) \
	$(ALIB)(unpk_temp41.o) \
	$(ALIB)(unpk_temp42.o) \
	$(ALIB)(unpk_temp420.o) \
	$(ALIB)(unpk_temp430.o) \
	$(ALIB)(unpk_temp48.o) \
	$(ALIB)(unpk_temp49.o) \
	$(ALIB)(unpk_trace.o) \
	$(ALIB)(unpkbg.o) \
	$(ALIB)(unpkbmap.o) \
	$(ALIB)(unpkcmbm.o) \
	$(ALIB)(unpklx.o) \
	$(ALIB)(unpklxbm.o) \
	$(ALIB)(unpkoo.o) \
	$(ALIB)(unpkpo.o) \
	$(ALIB)(unpkps.o) \
	$(ALIB)(unpksecdif.o)

ALIB = $(OS_LIB)/mdlg2dec.a
 
.PRECIOUS: $(ALIB)

all : $(ALIB)

$(ALIB): $(LOBJS) $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<


clean:
	$(RM) *.o

