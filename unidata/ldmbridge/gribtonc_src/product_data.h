/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: product_data.h,v 1.3 1995/06/01 05:06:50 russ Exp $ */

/*
 * Easy-to-access representation for a decoded GRIB product.  In this
 * structure, numbers have been converted to ints and floats, units have
 * been conventionalized, pole values have been replicated, etc.
 */

#ifndef PRODUCT_DATA_H_
#define PRODUCT_DATA_H_

#include "gdes.h"		/* GDS info in a more accessible grid
				   description structure */
#include "gbytem.h"		/* Bitmap Section info in a more accessible
				   structure */
#include "gbds.h"		/* Binary Data Section info in a more
				   accessible structure */
/*
 * Structure for GRIB product data, including the stuff of interest from the
 * Indicator Section, Product Definition Section, Grid Description Section
 * (manufactured, if necessary), and a bytemap (manufactured, if necessary).
 * We trade space for access ease in this structure. In particular, 1-, 2-,
 * and 3-byte sized numbers become ints.
 * "Missing" integers are set to GDES_INT_MISSING.
 * "Missing" floats are set to GDES_FLOAT_MISSING.
 */
struct product_data {
    char            delim[4];	/* "GRIB", indicator block */
    int             pdslen;     /* pds length...important for ensemble */
    char            *pdsloc;    /* pointer to the pds */
    int		    edition;	/* 0 or 1, so far */
    int             center;	/* NMC is 7 */
    int             model;	/* allocated by the originating center */
    int             grid;	/* Grid ID, may be center-specific */
    int		    param_vers;	/* parameter table version */
    int             param;	/* Indicator of parameter */
    int             level_flg ;	/* Indicator of type of level */
    int             level[2] ;
    int		    century;	/* ref time of data, =20 until 1 Jan 2001 */
    int             year;	/* of century */
    int             month;
    int             day;
    int             hour;
    int             minute;
    int             tunit;	/* Indicator of forecast time unit */
    int             tr[2];	/* Time range */
    int             tr_flg;	/* Time range flag */
    int             avg ;	/* when tr_flg indictes an average */
    int		    missing;	/* # missing from averages, accumulations */
    int		    has_gds;	/* 1 if a Grid Description Section was
                                   supplied, 0 otherwise */
    int		    has_bms;	/* 1 if a a Bit Map Section was supplied, 0
                                   otherwise */
    int		    scale10;	/* decimal scale factor exponent */
    int             cols;	/* 'columns' per 'row' */
    int             npts ;	/* # of floating point values */
    int             bits ;	/* # of bits used in GRIB encoding */
    char            *header;	/* WMO header (or manufactured product ID) */
    gdes	    *gd;	/* grid description */
    gbytem	    *bm;	/* byte map of values */
    gbds	    *bd;	/* binary data parameters */
    float           *data ;	/* unpacked data values */
};

typedef struct product_data product_data;

#ifdef __cplusplus
/* Fillin preallocated product_data from raw grib1 */
extern "C" int make_product_data(grib1*, product_data *);
/* Allocate a new product_data and fill in from raw grib1 */
extern "C" product_data* new_product_data(grib1*);
/* Free product_data */
extern "C" void free_product_data(product_data*);
#elif defined(__STDC__)
/* Fillin preallocated product_data from raw grib1 */
extern int make_product_data(grib1*, product_data *);
/* Allocate a new product_data and fill in from raw grib1 */
extern product_data* new_product_data(grib1*);
/* Free product_data */
extern void free_product_data(product_data*);
#else
/* Fillin preallocated product_data from raw grib1 */
extern int make_product_data( /* grib1*, product_data * */);
/* Allocate a new product_data and fill in from raw grib1 */
extern product_data* new_product_data( /* grib1* */ );
/* Free product_data */
extern void free_product_data( /* product_data* */ );
#endif

#endif /* PRODUCT_DATA_H_ */
