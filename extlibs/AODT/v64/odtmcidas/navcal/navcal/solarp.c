/* solarp.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b3 = 232700;
static integer c_b4 = 1011311;
static integer c__0 = 0;
static doublereal c_b8 = 360.;
static integer c__1 = 1;
static real c_b10 = 360.f;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* Subroutine */ int solarp_(integer *jday, integer *jtime, real *gha, real *
	dec, real *xlat, real *xlon)
{
    /* Initialized data */

    static integer init = 0;

    /* System generated locals */
    real r__1, r__2;
    doublereal d__1;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), sqrt(doublereal), atan2(
	    doublereal, doublereal), d_mod(doublereal *, doublereal *), r_mod(
	    real *, real *);

    /* Local variables */
    static integer i__;
    static real pi, px, py, pz, qx, qy, qz, xs, ys, zs, sha;
    static doublereal xha;
    static real cand;
    static doublereal raha;
    static real cinc, sand;
    static integer iday;
    static real cper, sinc, slra, xmmc, sper;
    extern real flalo_(integer *);
    static real rdpdg;
    extern real ftime_(integer *);
    static real xfact;
    static integer irayd, iepyd;
    static real ptime;
    static doublereal ecanm1;
    static real oeccen;
    static doublereal ecanom;
    static real asnode;
    extern real geolat_(real *, integer *);
    static doublereal diftim;
    extern doublereal timdif_(integer *, integer *, integer *, integer *);
    static real oincli, perhel, xomega, yomega;
    static integer irahms, iephms;
    static real epsiln, solsid;
    static doublereal xmanom;

/* *** $Id: solarp.f,v 1.1 2001/04/16 20:59:07 daves Exp $ *** */
/* SOLARP MOSHER 1074 WINLIB  Z HOUR ANGLE AND SOLAR DECL FOR DAY-TIME */
/* $ SUBROUTINE SOLARP(JDAY, JTIME, GHA, DEC, XLAT, XLON)  (DAS) */
/* $ COMPUTES GREENWICH HOUR ANGLE AND DECLINATION OF SUN */
/* $ JDAY = (I) INPUT  SATELLITE/YEAR/DAY */
/* $ JTIME = (I) INPUT  HOUR/MINUTE/SECOND */
/* $ GHA = (R) OUTPUT  GREENWICH HOUR ANGLE */
/* $ DEC = (R) OUTPUT  DECLINATION */
/* $ XLAT = (R) OUTPUT  LATITUDE OF SUN POSITION */
/* $ XLON = (R) OUTPUT  LONGITUDE OF SUN POSITION */
/* $$ SOLARP = COMPUTATION, NAVIGATION */

/*     ORBITAL CONSTANTS */

/*     IEPYD = EPOCH YEAR-DAY */
/*     IEPHMS = EPOCH HOUR-MINUTE-SECOND */
/*     OECCEN = ECCENTRICITY OF EARTH ORBIT */
/*     OINCLI = INCLINATION TO CELESTIAL EQUATOR */
/*     PERHEL = PERIHELION */
/*     ASNODE = ASCENDING NODE */
/*     XMANOM = MEAN ANOMOLY */
/*     XMMC = MEAN MOTION CONSTANT */
/*     SHA = CELESTIAL HOUR ANGLE */
/*     IRAYD  =  YYDDD WHEN CELESTIAL COOR. SYS. COINCIDES WITH EARTH COO */
/*     IRAHMS = HHMMSS WHEN CELESTIAL COOR. SYS. COINCIDES WITH EARTH COO */

/*     REAL*8 DABS,DMOD,DSQRT,DSIN,DCOS,DATAN2 */


    if (init != 0) {
	goto L1;
    }
    init = 1;
    pi = 3.14159265f;
    rdpdg = pi / 180.f;
    solsid = 1.00273791f;
    iepyd = 74004;
    iephms = 0;
    oeccen = .016722f;
    oincli = rdpdg * flalo_(&c_b3);
    perhel = rdpdg * flalo_(&c_b4) + pi;
    asnode = rdpdg * flalo_(&c__0);
    xmmc = 1.1945902048611111e-5f;
    sha = 100.26467f;
    irayd = 74001;
    irahms = 0;
    sinc = sin(oincli);
    cinc = cos(oincli);
    sper = sin(perhel);
    cper = cos(perhel);
    sand = sin(asnode);
    cand = cos(asnode);
    px = cper * cand - sper * sand * cinc;
    py = cper * sand + sper * cand * cinc;
    pz = sper * sinc;
    qx = -sper * cand - cper * sand * cinc;
    qy = -sper * sand + cper * cand * cinc;
    qz = cper * sinc;
L1:
    iday = *jday % 100000;
    ptime = ftime_(jtime);
    diftim = timdif_(&iepyd, &iephms, &iday, jtime);
    xmanom = xmmc * diftim;
    ecanm1 = xmanom;
    epsiln = 1e-8f;
    for (i__ = 1; i__ <= 20; ++i__) {
	ecanom = xmanom + oeccen * sin(ecanm1);
	if ((d__1 = ecanom - ecanm1, abs(d__1)) < epsiln) {
	    goto L3;
	}
/* L2: */
	ecanm1 = ecanom;
    }
L3:
    xomega = cos(ecanom) - oeccen;
/* Computing 2nd power */
    r__1 = oeccen;
    yomega = sqrt(1.f - r__1 * r__1) * sin(ecanom);
/* Computing 2nd power */
    r__1 = xomega;
/* Computing 2nd power */
    r__2 = yomega;
    xfact = 1.f / sqrt(r__1 * r__1 + r__2 * r__2);
    xomega *= xfact;
    yomega *= xfact;
    xs = xomega * px + yomega * qx;
    ys = xomega * py + yomega * qy;
    zs = xomega * pz + yomega * qz;
    slra = atan2(ys, xs) / rdpdg;
    raha = timdif_(&irayd, &irahms, &iday, jtime) * solsid / 4.f;
    *gha = ptime * 15.f;
    xha = 360.f - sha - raha + slra + *gha;
    *gha = d_mod(&xha, &c_b8);
    *gha = 360.f - *gha - 2.f;
/* Computing 2nd power */
    r__1 = xs;
/* Computing 2nd power */
    r__2 = ys;
    *dec = atan2(zs, sqrt(r__1 * r__1 + r__2 * r__2)) / rdpdg;
    r__1 = *dec * rdpdg;
    *xlat = geolat_(&r__1, &c__1) / rdpdg;
    *xlon = -(*gha) - ptime * 15.f + 720.f;
    *xlon = r_mod(xlon, &c_b10);
    return 0;
} /* solarp_ */

