/* llcart.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* Subroutine */ int llcart_0_(int n__, real *xlat, real *xlon, real *x, real 
	*y, real *z__, doublereal *drad, doublereal *decc, integer *iwest, 
	integer *icord, real *tlat, real *rad)
{
    /* Initialized data */

    static doublereal asq = 40683833.48;
    static doublereal bsq = 40410330.18;
    static doublereal ab = 40546851.22;
    static doublereal ecc = .081992;
    static doublereal eccsqr = .00672265;
    static doublereal rdpdg = .0174532925199;
    static integer kwest = -1;
    static integer kcord = 0;

    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal), cos(doublereal), sin(doublereal), atan2(
	    doublereal, doublereal), atan(doublereal);

    /* Local variables */
    static real a, r__, csln, cslt, tcos, ylat, snln, ylon, snlt, tnlt;
    static doublereal ddrad, dpole;

/* *** $Id: llcart.f,v 1.1 2000/06/26 15:51:48 daves Exp $ *** */
/* $ SUBROUTINE LLCART(XLAT, XLON, X,Y,Z)  (DAS) */
/* $ CONVERT LAT, LON TO CARTESIAN CENTERED COORDS  (X, Y, Z). */
/* $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH + */
/* $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST + */
/* $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF */
/* $  PLANET. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES */
/* $  EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE. */
/* $  (IN KM). */
/* $$ LLCART = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION */
    switch(n__) {
	case 1: goto L_llopt;
	case 2: goto L_llobl;
	case 3: goto L_cartll;
	}

    goto L55;

L_llopt:
/* $ SUBROUTINE LLOPT(DRAD,DECC,IWEST,ICORD) */
/* $ LLOPT IS USED TO INPUT RADIUS & ECCENTRICITY OTHER THAN EARTH */
/* $ AND DETERMINE + LONGITUDE CONVETION & PLANETOCENTRIC/DETIC */
/* $ DRAD = (R*8) INPUT  EQUATORIAL RADIUS */
/* $ DECC = (R*8) INPUT  ECCENTRICITY */
/* $ IWEST = (I) INPUT  >= 0    WEST POSITIVE, < 0 WEST NEGATIVE */
/* $ ICORD = (I) INPUT  >= 0 PLANETODETIC, < 0 PLANETOCENTRIC */
/* $$ LLOPT = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION */
    asq = *drad * *drad;
    ecc = *decc;
    eccsqr = ecc * ecc;
    dpole = sqrt(asq * (1. - eccsqr));
    bsq = dpole * dpole;
    ab = *drad * dpole;
    if (*iwest < 0) {
	kwest = 1;
    }
    if (*icord < 0) {
	kcord = -1;
    }
    return 0;

L_llobl:
/* $ SUBROUTINE LLOBL(TLAT,RAD) (DAS) */
/* $ CALCULATE RADIUS AT LATITUDE TLAT */
/* $ TLAT = (R) INPUT  LATITUDE */
/* $ RAD = (R) OUTPUT  RADIUS IN KM */
/* $$ LLOBL = NAVIGATION,COORDINATES */
    tcos = cos(*tlat * rdpdg);
    ddrad = (1. - eccsqr) * asq / (1. - eccsqr * tcos * tcos);
    *rad = sqrt(ddrad);
    return 0;
L55:
    ylat = rdpdg * *xlat;
/* -----CONVERT TO GEOCENTRIC (SPHERICAL) LATITUDE IF KCORD >= 0 */
/* CC     YLAT=GEOLAT(YLAT,1) */
    if (kcord >= 0) {
	ylat = atan2(bsq * sin(ylat), asq * cos(ylat));
    }
    ylon = kwest * rdpdg * *xlon;
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

L_cartll:
/* $ SUBROUTINE CARTLL(X, Y, Z, XLAT, XLON)  (DALY 1978) */
/* $ CONVERT CARTESIAN CENTERED COORD (X, Y, Z) TO LAT, LON. */
/* $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF */
/* $  PLANET. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES */
/* $  EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE. */
/* $  IN KM. */
/* $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH + */
/* $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST + */
/* $$ CARTLL = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION */


    *xlat = 100.f;
    *xlon = 200.f;
    if (*x == 0.f && *y == 0.f && *z__ == 0.f) {
	goto L90;
    }
    a = atan(*z__ / sqrt(*x * *x + *y * *y));
/* -----CONVERT TO GEODETIC LATITUDE IF KCORD > 0 */
/* CC     XLAT=GEOLAT(ATAN(Z/SQRT(X*X+Y*Y)),2)/RDPDG */
    if (kcord >= 0) {
	*xlat = atan2(asq * sin(a), bsq * cos(a)) / rdpdg;
    } else {
	*xlat = a / rdpdg;
    }
    *xlon = kwest * atan2(*y, *x) / rdpdg;
L90:
    return 0;
} /* llcart_ */

/* Subroutine */ int llcart_(real *xlat, real *xlon, real *x, real *y, real *
	z__)
{
    return llcart_0_(0, xlat, xlon, x, y, z__, (doublereal *)0, (doublereal *)
	    0, (integer *)0, (integer *)0, (real *)0, (real *)0);
    }

/* Subroutine */ int llopt_(doublereal *drad, doublereal *decc, integer *
	iwest, integer *icord)
{
    return llcart_0_(1, (real *)0, (real *)0, (real *)0, (real *)0, (real *)0,
	     drad, decc, iwest, icord, (real *)0, (real *)0);
    }

/* Subroutine */ int llobl_(real *tlat, real *rad)
{
    return llcart_0_(2, (real *)0, (real *)0, (real *)0, (real *)0, (real *)0,
	     (doublereal *)0, (doublereal *)0, (integer *)0, (integer *)0, 
	    tlat, rad);
    }

/* Subroutine */ int cartll_(real *x, real *y, real *z__, real *xlat, real *
	xlon)
{
    return llcart_0_(3, xlat, xlon, x, y, z__, (doublereal *)0, (doublereal *)
	    0, (integer *)0, (integer *)0, (real *)0, (real *)0);
    }

