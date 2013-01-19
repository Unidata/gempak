/* gryscl.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: gryscl.f,v 1.1 2000/07/12 13:12:23 gad Exp $ *** */
/* $ Name: */
/* $      gryscl  - Converts a brightness temperature to a grey scale. */
/* $ */
/* $ Interface: */
/* $      subroutine */
/* $      gryscl(real tempk, integer brit) */
/* $ */
/* $ Input: */
/* $      tempk   - Temperature being converted in degrees Kelvin. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      brit    - Brightness value. */
/* $ */
/* $ Return values: */
/* $      none */
/* $ */
/* $ Remarks: */
/* $      none */
/* $ */
/* $ Categories: */
/* $      converter */
/* $      calibration */
/* Subroutine */ int gryscl_(real *tempk, integer *ibrit)
{
    /* Initialized data */

    static integer con1 = 418;
    static integer con2 = 660;
    static real tlim = 242.f;

    /* System generated locals */
    integer i__1;


/* --- CONVERT A BRIGHTNESS TEMPERATURE TO A GREY SCALE */
/*     TEMPK---TEMPERATURE IN DEGREES KELVIN */

    if (*tempk < tlim) {
/* Computing MIN */
	i__1 = con1 - (integer) (*tempk);
	*ibrit = min(i__1,255);
    } else {
/* Computing MAX */
	i__1 = con2 - (integer) (*tempk * 2);
	*ibrit = max(i__1,0);
    }

    return 0;
} /* gryscl_ */

