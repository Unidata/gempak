## Use automake to generate Makefile.in from this file ##

AUTOMAKE_OPTIONS = foreign

INCLUDES = -I$(prefix)/include
LOCAL_LIB = ../.libs/librsl.a
LDADD = @LIBS@ $(LOCAL_LIB) 
bin_PROGRAMS = any_to_gif any_to_uf qlook
any_to_gif_LDFLAGS = -static
any_to_uf_LDFLAGS = -static
# Additional program to build but not install
noinst_PROGRAMS = any_to_ppm any_to_ufgz bscan \
 cappi_image dorade_main killer_sweep \
 kwaj_subtract_one_day lassen_to_gif print_hash_table \
 print_header_info sector test_get_win \
 wsr88d_to_gif wsr_hist_uf_test 


# Only this one has more than one source.  All the others use the
# default *.c format.

qlook_SOURCES = qlook.c qlook_usage.c adjust_gate_size.c

EXTRA_DIST = run_tests
