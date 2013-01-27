/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: ncfloat.h,v 1.1 1995/06/01 05:06:44 russ Exp $ */

/* GRIB nc_float */

#ifndef NC_FLOAT_H_
#define NC_FLOAT_H_

#ifdef __cplusplus
extern "C" int nc_units(int ncid, int varid, char *units,
			double *slope, double *intercept);
extern "C" int nc_float(int ncid, int varid, long *corn, long *edge,
			float *data, double slope, double intercept,
			float missing);
extern "C" int float_nc(int ncid, int varid, long *corn, long *edge,
			float *data, double slope, double intercept,
			float missing);
extern "C" int float_ncg(int ncid, int varid, long *corn, long *edge,
			 long *stride, long *imap, float *data, double slope,
			 double intercept, float missing);
#elif defined(__STDC__)
extern char* modelname(int center, int model);
#else
#define const
extern char* modelname( /* int center, int model */ );
#endif

#endif /* NC_FLOAT_H_ */
