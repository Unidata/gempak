/* m0itime.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: m0itime.f,v 1.1 2001/04/16 20:59:07 daves Exp $ *** */
/* $ FUNCTION M0ITIME(X)  (JMB) */
/* $ FLOATING POINT TIME TO PACKED INTEGER ( SIGN HH MM SS ) */
/* $ X = (R) INPUT  FLOATING POINT TIME */
integer m0itime_(real *x)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer i__, j;
    static real y;

/* --- local variables */
    if (*x < 0.f) {
	goto L1;
    }
    y = *x;
    i__ = 1;
    goto L2;
L1:
    y = -(*x);
    i__ = -1;
L2:
    j = y * 3600.f + .5f;
    ret_val = j / 3600 * 10000 + j / 60 % 60 * 100 + j % 60;
    ret_val = i__ * ret_val;
    return ret_val;
} /* m0itime_ */

