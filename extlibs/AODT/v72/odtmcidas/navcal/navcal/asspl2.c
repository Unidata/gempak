/* asspl2.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* Subroutine */ int asspl2_(integer *n, real *x, real *y, real *c__)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, j, m;
    static real s, t, u, v, w, z__, aa, ba, ca, da, wq, ws, zq, zs;

/* *** $Id: asspl2.f,v 1.1 2000/07/24 13:50:40 gad Exp $ *** */
/* NP   ASSPL2 */
/* * CALLED BY 'SPLINE' */
/* $ CALL ASSPL2(N, X, Y, C)  (DAS) */
/* $ CUBIC SPLINE GENERATOR.  SECOND DERIV. NOT CONTINUOUS.  THE DEGREES */
/* $   OF FREEDOM GAINED ARE USED IN AN ATTEMPT TO AVOID  OSCILLATIONS. */
/* $   FIT TO THE POINTS (X(I),Y(I)) I=1,...,N . */
/* $ INPUT: */
/* $   N = (I) THE NUMBER OF DATA POINTS */
/* $   X = (R) ARRAY OF THE ABSCISSA OF POINTS */
/* $   Y = (R) ARRAY OF THE ORDINATE OF POINTS */
/* $ OUTPUT: */
/* $   C = (R) ARRAY OF CUBIC SPLINE COEFFICIENTS */
/* $$ ASSPL2 = COMPUTATION */
    /* Parameter adjustments */
    c__ -= 5;
    --y;
    --x;

    /* Function Body */
    m = *n - 1;
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    goto L110;
	}
	t = (y[i__ + 1] - y[i__ - 1]) / (x[i__ + 1] - x[i__ - 1]);
	goto L120;
L110:
	w = (y[2] + y[3]) / 2.f;
	z__ = (x[2] + x[3]) / 2.f;
	t = (w - y[1]) / (z__ - x[1]);
	t = (y[2] - y[1]) * 2.f / (x[2] - x[1]) - t;
L120:
	if (i__ == m) {
	    goto L130;
	}
	s = (y[i__ + 2] - y[i__]) / (x[i__ + 2] - x[i__]);
	goto L140;
L130:
	w = (y[*n - 1] + y[*n - 2]) / 2.f;
	z__ = (x[*n - 1] + x[*n - 2]) / 2.f;
	s = (y[*n] - w) / (x[*n] - z__);
	s = (y[*n] - y[*n - 1]) * 2.f / (x[*n] - x[*n - 1]) - s;
L140:
	u = y[i__ + 1];
	v = y[i__];
	w = (x[i__ + 1] + x[i__]) / 2.f;
	z__ = (x[i__ + 1] - x[i__]) / 2.f;
	zs = z__ * z__;
	zq = z__ * zs;
	ws = w * w;
	wq = w * ws;
	aa = (u + v) * .5f - z__ * .25f * (s - t);
	ba = (u - v) * .75f / z__ - (s + t) * .25f;
	ca = (s - t) * .25f / z__;
	da = (s + t) * .25f / zs - (u - v) * .25f / zq;
	c__[(i__ << 2) + 1] = aa - ba * w + ca * ws - da * wq;
	c__[(i__ << 2) + 2] = ba - ca * 2.f * w + da * 3.f * ws;
	c__[(i__ << 2) + 3] = ca - da * 3.f * w;
	c__[(i__ << 2) + 4] = da;
/* L150: */
    }
    for (j = 1; j <= 4; ++j) {
	c__[j + (*n << 2)] = c__[j + (*n - 1 << 2)];
/* L44: */
    }
    return 0;
} /* asspl2_ */

real fval_(integer *n, real *u, real *x, real *c__)
{
    /* Initialized data */

    static integer i__ = 1;

    /* System generated locals */
    real ret_val;

    /* Local variables */
    static integer j, k;

/* $ FVAL(N, U, X, C)  (WBH) */
/* $ EVALUATES THE CUBIC SPLINE COEFFICIENTS AT A POINT */
/* $ N = (I) INPUT  NUMBER OF DATA POINTS */
/* $ U = (R) INPUT POINT TO EVALUATE */
/* $ X = (R) INPUT  ARRAY OF ABSCISSA OF POINTS */
/* $ C = (R) OUTPUT  ARRAY OF CUBIC SPINE COEFFICIENT */
/* $$ FVAL = COMPUTATION */
    /* Parameter adjustments */
    c__ -= 5;
    --x;

    /* Function Body */
    if (i__ >= *n) {
	i__ = 1;
    }
    if (*u < x[i__]) {
	goto L10;
    }
    if (*u <= x[i__ + 1]) {
	goto L30;
    }
L10:
    i__ = 1;
    j = *n + 1;
L20:
    k = (i__ + j) / 2;
    if (*u < x[k]) {
	j = k;
    }
    if (*u >= x[k]) {
	i__ = k;
    }
    if (j > i__ + 1) {
	goto L20;
    }
L30:
    ret_val = c__[(i__ << 2) + 1] + *u * (c__[(i__ << 2) + 2] + *u * (c__[(
	    i__ << 2) + 3] + *u * c__[(i__ << 2) + 4]));
    return ret_val;
} /* fval_ */

