/* nllxyz.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* Subroutine */ int nllxyz_(real *xlat, real *xlon, real *x, real *y, real *
	z__)
{
    /* Initialized data */

    static real asq = 40683833.48f;
    static real bsq = 40410330.18f;
    static real ab = 40546851.22f;
    static real rdpdg = .017453292f;

    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), atan2(doublereal, doublereal), 
	    sqrt(doublereal);

    /* Local variables */
    static real r__, csln, cslt, ylat, snln, ylon, snlt, tnlt;

/* *** $Id: nllxyz.f,v 1.1 2000/07/24 13:50:41 gad Exp $ *** */
/* $ SUBROUTINE NLLXYZ(XLAT, XLON, XYZ)  (DAS) */
/* $ CONVERT LAT, LON TO EARTH CENTERED COORDS  (X, Y, Z). */
/* $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH + */
/* $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST + */
/* $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF */
/* $   EARTH. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES */
/* $   EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE. */
/* $   (IN KM). */
/* $$ NLLXYZ = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION */
    ylat = rdpdg * *xlat;
/* -----CONVERT TO GEOCENTRIC (SPHERICAL) LATITUDE */
/* CC     YLAT=GEOLAT(YLAT,1) */
    ylat = atan2(bsq * sin(ylat), asq * cos(ylat));
    ylon = -rdpdg * *xlon;
    snlt = sin(ylat);
    cslt = cos(ylat);
    csln = cos(ylon);
    snln = sin(ylon);
/* Computing 2nd power */
    r__1 = snlt / cslt;
    tnlt = r__1 * r__1;
    r__ = ab * sqrt((tnlt + 1.f) / (bsq + asq * tnlt));
    *x = r__ * cslt * csln;
    *y = r__ * cslt * snln;
    *z__ = r__ * snlt;
    return 0;
} /* nllxyz_ */

/* Subroutine */ int nxyzll_(real *x, real *y, real *z__, real *xlat, real *
	xlon)
{
    /* Initialized data */

    static real rdpdg = .01745329252f;
    static real asq = 40683833.48f;
    static real bsq = 40410330.18f;

    /* Builtin functions */
    double sqrt(doublereal), atan(doublereal), sin(doublereal), cos(
	    doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    static real a;

/* $ SUBROUTINE NXYZLL(X, Y, Z, XLAT, XLON)  (DAS) */
/* $ CONVERT EARTH CENTERED COORD (X, Y, Z) TO LAT, LON. */
/* $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF */
/* $   EARTH. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES */
/* $   EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE. */
/* $   IN KM. */
/* $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH + */
/* $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST + */
/* $$ NXYZLL = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION */


    *xlat = 100.f;
    *xlon = 200.f;
    if (*x == 0.f && *y == 0.f && *z__ == 0.f) {
	goto L90;
    }
    a = atan(*z__ / sqrt(*x * *x + *y * *y));
/* -----CONVERT TO GEODETIC LATITUDE */
/* CC     XLAT=GEOLAT(ATAN(Z/SQRT(X*X+Y*Y)),2)/RDPDG */
    *xlat = atan2(asq * sin(a), bsq * cos(a)) / rdpdg;
    *xlon = -atan2(*y, *x) / rdpdg;
L90:
    return 0;
} /* nxyzll_ */

