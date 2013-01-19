/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: units.h,v 1.6 1995/11/22 18:16:18 russ Exp $ */

/* Functions for dealing with udunits and GRIB units */

#ifndef UNITS_H_
#define UNITS_H_

#include <udunits.h>
#include "product_data.h"
#include "nc.h"

#ifndef UNITS_NAME
#define UNITS_NAME	"units"	/* netCDF name of units attribute */
#endif

#ifdef __cplusplus
extern "C" int init_udunits(void);
extern "C" int rvhours(product_data *, ncfile *,
		       double *reftime, double *valtime);
extern "C" int get_units(int ncid, int varid, utUnit **unitsp);
extern "C" unitconv *uconv(char *varname, utUnit *units);
#elif defined(__STDC__)
extern int init_udunits();
extern int rvhours(product_data *, ncfile *,
		   double *reftime, double *valtime);
extern int get_units(int ncid, int varid, utUnit **unitsp);
extern unitconv *uconv(char *varname, utUnit *units);
#else
extern int init_udunits();
extern int rvhours( /* product_data *, ncfile *,
		       double *reftime, double *valtime */);
extern int get_units(/* int ncid, int varid, utUnit **unitsp */);
extern unitconv *uconv( /* char *varname, utUnit *units */ );
#endif

#endif /* UNITS_H_ */
