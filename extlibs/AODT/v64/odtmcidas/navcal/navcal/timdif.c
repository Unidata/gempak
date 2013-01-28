/* timdif.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
doublereal timdif_(integer *iyrda1, integer *ihms1, integer *iyrda2, integer *
	ihms2)
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern integer mcydtocyd_(integer *, integer *);
    static integer iy1, iy2;
    static doublereal timedif_sec__;
    static integer iret, isec1, isec2;
    extern integer mcdaytimetosec_(integer *, integer *, integer *);

/* *** $Id: timdif.f,v 1.1 2001/04/16 20:59:07 daves Exp $ *** */
/* $ Name: */
/* $      timdif - Returns difference between two times in minutes. */
/* $ */
/* $ Interface: */
/* $      double precision function */
/* $      timdif(integer iyrda1, integer ihms1, integer iyrda2, */
/* $             integer ihms2) */
/* $ */
/* $ Input: */
/* $      iyrda1  - Year/day of first time (yyddd). */
/* $      ihms1   - Hours/minutes/seconds of first time (hhmmss). */
/* $      iyrda2  - Year/day of second time (yyddd). */
/* $      ihms2   - Hours/minutes/seconds of second time (hhmmss). */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $      param2  - description of it */
/* $ */
/* $ Output: */
/* $      none */
/* $      param3  - description of it */
/* $ */
/* $ Return values: */
/* $      The difference between the two times, in minutes. */
/* $      If the first time is greater than the second, the result */
/* $      will be negative. */
/* $ */
/* $ Remarks: */
/* $      none */
/* $ */
/* $ Categories: */
/* $      converter */
/* $      day/time */
    iret = mcydtocyd_(iyrda1, &iy1);
    iret = mcydtocyd_(iyrda2, &iy2);
    iret = mcdaytimetosec_(&iy1, ihms1, &isec1);
    iret = mcdaytimetosec_(&iy2, ihms2, &isec2);
    timedif_sec__ = (doublereal) (isec2 - isec1);
    ret_val = timedif_sec__ / 60;
    return ret_val;
} /* timdif_ */

