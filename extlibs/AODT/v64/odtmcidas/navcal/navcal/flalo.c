/* flalo.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: flalo.f,v 1.1 2000/07/24 13:50:40 gad Exp $ *** */
/* $ Name: */
/* $      flalo - Converts a packed integer (SIGN DDD MM SS) latitude/longitude */
/* $              to real. */
/* $ */
/* $ Interface: */
/* $      real function */
/* $      flalo(integer m) */
/* $ */
/* $ Input: */
/* $      m     - Integer containing the packed data. */
/* $ */
/* $ Input and Output: */
/* $	none */
/* $ */
/* $ Output: */
/* $	none */
/* $ */
/* $ Return values: */
/* $            - Converted representation of a lat/lon. */
/* $ */
/* $ */
/* $ Remarks: */
/* $      No error checking is performed. If the input is not formatted */
/* $      correctly the output will be inaccurate. */
/* $ */
/* $ Categories: */
/* $	converter */

real flalo_(integer *m)
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
} /* flalo_ */

