## This is a automake file, part of Unidata's netCDF package.
# Copyright 2010, see the COPYRIGHT file for more information.

# This automake file generates the Makefile to build the include
# directory.

include_HEADERS = netcdf.h netcdf_meta.h

if BUILD_PARALLEL
include_HEADERS += netcdf_par.h
endif

noinst_HEADERS = nc_logging.h nc_tests.h fbits.h nc.h	\
nclist.h ncuri.h utf8proc.h ncdispatch.h ncdimscale.h		\
netcdf_f.h err_macros.h ncbytes.h nchashmap.h ceconstraints.h rnd.h	\
nclog.h ncconfigure.h nc4internal.h nctime.h nc3dispatch.h nc3internal.h \
onstack.h

if USE_NETCDF4
noinst_HEADERS += ncaux.h
endif

EXTRA_DIST = CMakeLists.txt XGetopt.h netcdf_meta.h.in
