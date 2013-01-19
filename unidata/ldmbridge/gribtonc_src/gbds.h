/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: gbds.h,v 1.4 1996/01/10 16:21:59 russ Exp $ */

/*
 * NMC "GRIB" Edition 1 Binary Data Section info in a more accessible form
 */

#ifndef _GBDS_H
#define _GBDS_H

#include "grib1.h"

#define FILL_VAL	-9999.	/* to fill in missing values */

typedef struct gbds {
    int bscale;			/* binary scale factor (E) */
    float ref;			/* reference value (minimum of packed
				   values) */
    int nbits;			/* number of bits containing each packed
				   value */
    int unused;			/* number of unused bits at end of data
				   section */
				/* The following is_xxx members are from
                                   GRIB Code Table 11 */
    int nvals;			/* Number of values in binary data section, 0
				   if values all constant reference value */
    int is_sph_har;		/* 1 if binary data is spherical harmonics
				   coefficients, 0 if grid point data */
    int is_not_simple;		/* 1 if complex (for spherical harmonics) or
				   second-order packing (for grid-point
				   data) is used, 0 if simple packing */
    int is_ints;		/* 1 if integer values (in the original
				   data) are represented, 0 if
				   floating-point values are represented */
				/* The following is_xxx have meaning only
                                   for grid-point data that uses
                                   second-order packing */
    int has_matrix;		/* 1 if matrix of values at each grid point,
				   0 if single datum at each grid point */
    int has_secbm;		/* 1 if second-order bit map present, 0 if no
				   second-order bit map */
    int has_dif_widths;		/* 1 if second-order values different
				   widths, 0 if second-order values constant
				   widths */
    unsigned char *packed;      /* the packed data */
} gbds;

#ifdef __cplusplus
extern "C" gbds* make_gbds(bds*); /* make binary data structure */
extern "C" void free_gbds(gbds*); /* free binary data structure */
/* Unpack data from binary data section */
extern "C" float* unpackbds(gbds*, gbytem*, int npts, int scale10);#
#elif defined(__STDC__)
extern gbds* make_gbds(bds*); /* make binary data structure */
extern void free_gbds(gbds*); /* free binary data structure */
/* Unpack data from binary data section */
extern float* unpackbds(gbds*, gbytem*, int npts, int scale10);
#else
extern gbds* make_gbds( /* bds* */ ); /* make binary data structure */
extern void free_gbds( /* gbds* */ ); /* free binary data structure */
/* Unpack data from binary data section */
extern float* unpackbds( /* gbds*, gbytem*, int npts, int scale10 */ );
#endif

#endif /* _GBDS_H */
