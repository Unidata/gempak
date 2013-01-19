/* raerac.f -- translated by f2c (version 20000817).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 360.;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
doublereal raerac_(iyrdy, ihms, rae)
integer *iyrdy, *ihms;
real *rae;
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double d_mod();

    /* Local variables */
    static doublereal raha;
    static integer irayd;
    extern doublereal timdif_();
    static integer irahms;
    static doublereal solsid;
    static real rac;
    static doublereal sha;

/* *** $Id: raerac.f,v 1.6 1997/10/10 20:18:42 dglo Exp $ *** */
/* RAERAC PHILLI 0174 NAV: CONVERTS EARTH LON TO CELESTIAL LON */
/* $ FUNCTION RAERAC(IYRDY, IHMS, RAE)  (DAS) */
/* $ CONVERT CONVERT EARTH LONGITUDE TO CELESTIAL LONGITUDE. FN VAL IS */
/* $   IN REAL*4 DEGREES. */
/* $ IYRDY = (I) INPUT  YEAR AND JULIAN DAY (YYDDD) */
/* $ IHMS = (I) INPUT  TIME (HHMMSS) */
/* $ RAE = (R) INPUT  EARTH LONGITUDE (DEGREES) */
/* $$ RAERAC = NAVIGATION, CONVERT, LONGITUDE */

/* REF: ESCOBAL, "METHODS OF ORBIT DETERMINATION." WILEY & SONS, 1965 */
/* GREENWICH SIDEREAL TIME := ANGLE BETWEEN PRIME MERID. & 0 DEG R.A. */
/* JULIAN DATE := # DAYS ELAPSED SINCE 12 NOON ON JAN 1, 4713 B.C. */
/* APPROXIMATE FORMULA FOR GREENWICH SIDEREAL TIME AT 0Z: */
/* G.S.T (DEG) = S(0) = 99.6909833 + 36000.7689*C + 0.00038708*C*C ,WHERE */
/*    C = TIME IN CENTURIES = (J.D. - 2415020.0) / 36525 */
/* FOR G.S.T. AT OTHER TIMES OF (SAME) DAY, USE: */
/* G.S.T AT TIME T = S(T) = S(0) + (T * DS/DT) */
/* DS/DT = 1 + (1 / 365.24219879) = 1.00273790927 REVOLUTIONS/DAY */
/*       = 0.250684477 DEGREES/MINUTE */

/* FROM TABLE, J.D. AT 0Z ON JAN 1,1974 = 2442048.5 */
/* THEN S(0) AT 0Z ON JAN 1,1974 = 100.2601800 */


    sha = 100.26467;
    irayd = 74001;
    irahms = 0;
    solsid = 1.00273791;
    raha = *rae + timdif_(&irayd, &irahms, iyrdy, ihms) * solsid / 4. + sha;
    rac = d_mod(&raha, &c_b2);
    if (rac < (float)0.) {
	rac += (float)360.;
    }
    ret_val = rac;
    return ret_val;
} /* raerac_ */



doublereal racrae_(iyrdy, ihms, rac)
integer *iyrdy, *ihms;
real *rac;
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double d_mod();

    /* Local variables */
    static doublereal raha;
    static integer irayd;
    extern doublereal timdif_();
    static integer irahms;
    static doublereal solsid;
    static real rae;
    static doublereal sha;

/* RACRAE PHILLI 0174 NAV: CONVERTS CELESTIAL ONG TO EARTH LON */
/* $ FUNCTION RACRAE(IYRDY, IHMS, RAC)  (DAS) */
/* $ CONVERT CONVERT CELESTIAL LONGITUDE TO EARTH LONGITUDE. FN VAL IS */
/* $   IN REAL*4 DEGREES. */
/* $ IYRDY = (I) INPUT  YEAR AND JULIAN DAY (YYDDD) */
/* $ IHMS = (I) INPUT  TIME (HHMMSS) */
/* $ RAE = (R) INPUT  CELESTIAL LONGITUDE (DEGREES) */
/* $$ RACRAE = NAVIGATION, CONVERT, LONGITUDE */


    sha = 100.26467;
    irayd = 74001;
    irahms = 0;
    solsid = 1.00273791;
    raha = *rac - sha + timdif_(iyrdy, ihms, &irayd, &irahms) * solsid / 4.;
    rae = d_mod(&raha, &c_b2);
    if (rae < (float)0.) {
	rae += (float)360.;
    }
    ret_val = rae;
    return ret_val;
} /* racrae_ */

