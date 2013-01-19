/* ftime.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: ftime.f,v 1.1 2001/04/16 20:59:07 daves Exp $ *** */
/* $ FUNCTION FTIME(M)  (BL) */
/* $ CONVERT PACKED INTEGER (SIGN HH MM SS) TIME TO REAL */
/* $ M = (I) INPUT  PACKED INTEGER (SIGN HH MM SS) TIME */
/* $$ FTIME = CONVERT, INTEGER, TIME, REAL */

/* $ Name: */
/* $      ftime   - Converts a packed integer time value to a real. */
/* $ */
/* $ Interface: */
/* $      real function */
/* $      ftime(integer m) */
/* $ */
/* $ Input: */
/* $      m       - Time, formatted as: */
/* $                 sign hhmmss. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      none */
/* $ */
/* $ Return values: */
/* $      The reformatted time. */
/* $ */
/* $ Remarks: */
/* $      The reformatted time is in terms of hours and fractions of */
/* $      hours.  93500 is returned as 9.5833. */
/* $ */
/* $      The sign can be negative. */
/* $ */
/* $ Categories: */
/* $      utility */
/* $      converter */
/* $      day/time */
real ftime_(integer *m)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static integer n;
    static real x;

    if (*m < 0) {
	goto L1;
    }
    n = *m;
    x = 1.f;
    goto L2;
L1:
    n = -(*m);
    x = -1.f;
L2:
    ret_val = (real) (n / 10000) + (real) (n / 100 % 100) / 60.f + (real) (n %
	     100) / 3600.f;
    ret_val = x * ret_val;
    return ret_val;
} /* ftime_ */

