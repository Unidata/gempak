/* movwc.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: movwc.f,v 1.1 2000/06/26 15:51:48 daves Exp $ *** */
/* $ Name: */
/* $      movwc  - Moves bytes in an integer array to a character array. */
/* $ */
/* $ Interface: */
/* $      subroutine */
/* $      movwc(integer ibuf, character*(*) cbuf)) */
/* $ */
/* $ Input: */
/* $      ibuf   - Array containing the bytes to be transferred. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      cbuf   - Array that will contain the transferred bytes. */
/* $ */
/* $ Return values: */
/* $      none */
/* $ */
/* $ Remarks: */
/* $      The number of bytes transferred depends on the length of */
/* $      the character array. */
/* $ */
/* $ Categories: */
/* $      utility */
/* Subroutine */ int movwc_(integer *ibuf, char *cbuf, ftnlen cbuf_len)
{
    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    static integer lenc;
    extern /* Subroutine */ int movb_(integer *, integer *, char *, integer *,
	     ftnlen);

    /* Parameter adjustments */
    --ibuf;

    /* Function Body */
    lenc = i_len(cbuf, cbuf_len);
    movb_(&lenc, &ibuf[1], cbuf, &c__0, cbuf_len);
    return 0;
} /* movwc_ */

