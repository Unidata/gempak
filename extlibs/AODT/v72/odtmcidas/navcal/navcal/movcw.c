/* movcw.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: movcw.f,v 1.1 2000/07/12 13:12:26 gad Exp $ *** */
/* $ Name: */
/* $      movcw - Moves characters in a string to an integer array. */
/* $ */
/* $ Interface: */
/* $      subroutine */
/* $      movcw(character*(*)cbuf, integer ibuf(*)) */
/* $ */
/* $ Input: */
/* $      cbuf  - String of characters, starting at first location. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      ibuf  - Integer array receiving characters. */
/* $ */
/* $ Return values: */
/* $      none */
/* $ */
/* $ Remarks: */
/* $      The number of characters transferred depends on the length of */
/* $      the input character array. */
/* $ */
/* $ Categories: */
/* $      utility */
/* Subroutine */ int movcw_(char *cbuf, integer *ibuf, ftnlen cbuf_len)
{
    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    static integer lenc;
    extern /* Subroutine */ int movb_(integer *, char *, integer *, integer *,
	     ftnlen);

    /* Parameter adjustments */
    --ibuf;

    /* Function Body */
    lenc = i_len(cbuf, cbuf_len);
    movb_(&lenc, cbuf, &ibuf[1], &c__0, cbuf_len);
    return 0;
} /* movcw_ */

