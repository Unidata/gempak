#
#	Makefile for GEMPAK7
#		Builds program makefiles
#		Unidata 11/99 Chiz
#
#################################
include $(CONFIGDIR)/$(MAKEINC)

COBJS = \
	$(ALIB)(pk_swap.o)

LOBJS = \
	$(ALIB)(boustro_flt.o) \
	$(ALIB)(boustro_int.o) \
	$(ALIB)(check_flt.o) \
	$(ALIB)(check_int.o) \
	$(ALIB)(earth.o) \
	$(ALIB)(flr.o) \
	$(ALIB)(flt_map.o) \
	$(ALIB)(fmkieee.o) \
	$(ALIB)(int_map.o) \
	$(ALIB)(length.o) \
	$(ALIB)(pack_gp.o) \
	$(ALIB)(pack_opt.o) \
	$(ALIB)(pk_azimuth.o) \
	$(ALIB)(pk_bmap.o) \
	$(ALIB)(pk_c7.o) \
	$(ALIB)(pk_cmplx.o) \
	$(ALIB)(pk_cylinder.o) \
	$(ALIB)(pk_endian.o) \
	$(ALIB)(pk_equator.o) \
	$(ALIB)(pk_grib2.o) \
	$(ALIB)(pk_lambert.o) \
	$(ALIB)(pk_mercator.o) \
	$(ALIB)(pk_missp.o) \
	$(ALIB)(pk_nomiss.o) \
	$(ALIB)(pk_orthographic.o) \
	$(ALIB)(pk_polster.o) \
	$(ALIB)(pk_s7.o) \
	$(ALIB)(pk_sect0.o) \
	$(ALIB)(pk_sect1.o) \
	$(ALIB)(pk_sect2.o) \
	$(ALIB)(pk_sect3.o) \
	$(ALIB)(pk_sect4.o) \
	$(ALIB)(pk_sect5.o) \
	$(ALIB)(pk_sect6.o) \
	$(ALIB)(pk_sect7.o) \
	$(ALIB)(pk_sect8.o) \
	$(ALIB)(pk_smple.o) \
	$(ALIB)(pk_temp40.o) \
	$(ALIB)(pk_temp41.o) \
	$(ALIB)(pk_temp42.o) \
	$(ALIB)(pk_temp420.o) \
	$(ALIB)(pk_temp430.o) \
	$(ALIB)(pk_temp48.o) \
	$(ALIB)(pk_temp49.o) \
	$(ALIB)(pk_trace.o) \
	$(ALIB)(pkbg.o) \
	$(ALIB)(prep_flt.o) \
	$(ALIB)(prep_int.o) \
	$(ALIB)(prep_noval.o) \
	$(ALIB)(prep_sect2_int.o) \
	$(ALIB)(prep_sect2_real.o) \
	$(ALIB)(prepr.o) \
	$(ALIB)(reduce.o) \
	$(ALIB)(timpr.o)

ALIB = $(OS_LIB)/mdlg2enc.a
 
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

