/* geolat.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
real geolat_(real *xlat, integer *idir)
{
    /* Initialized data */

    static real a = 6378.388f;
    static real b = 6356.912f;

    /* System generated locals */
    real ret_val, r__1;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    static real cx, sx, asq, bsq;

/* *** $Id: geolat.f,v 1.1 2001/04/16 20:59:06 daves Exp $ *** */
/* $ FUNCTION GEOLAT(XLAT, IDIR)  (DAS) */
/* $ GEOCENTRIC/GEODETIC LATITUDE CONVERSION.  FN VAL IN RADIANS. */
/* $ XLAT = (R) INPUT  LATITUDE (RADIANS) */
/* $ IDIR = (I) 1 FOR GEODEDIC TO GEOCENTRIC CONVERSION, 2 FOR GEOCENTRIC */
/* $   TO GEODEDIC CONVERSION */
/* $$ GEOLAT = CONVERT, LATITUDE, NAVIGATION */

/* -----XLAT, FN VALUE EXPRESSED IN RADIANS AS PER HARRIS SYSTEM */

/* Computing 2nd power */
    r__1 = a;
    asq = r__1 * r__1;
/* Computing 2nd power */
    r__1 = b;
    bsq = r__1 * r__1;
    cx = cos(*xlat);
    sx = sin(*xlat);
    if (*idir == 2) {
	goto L1;
    }
    ret_val = atan2(bsq * sx, asq * cx);
    return ret_val;
L1:
    ret_val = atan2(asq * sx, bsq * cx);
    return ret_val;
} /* geolat_ */

