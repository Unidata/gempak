/* cfz.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: cfz.f,v 1.1 2001/04/16 20:59:07 daves Exp $ *** */
/* $ Name: */
/* $      cfz     - Returns a hexadecimal representation of the data in an */
/* $                integer. */
/* $ */
/* $ Interface: */
/* $      character*12 function */
/* $      cfz(integer l) */
/* $ */
/* $ Input: */
/* $      l       - Field whose data is to be converted. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      none */
/* $ */
/* $ Return values: */
/* $      Character string representation of the data field as hexadecimal, */
/* $      right justified, with blank fill. */
/* $ */
/* $ Remarks: */
/* $      none */
/* $ */
/* $ Categories: */
/* $      text */
/* $      converter */
/* Character */ VOID cfz_(char *ret_val, ftnlen ret_val_len, integer *l)
{
    /* Format strings */
    static char fmt_1[] = "(4x,z8)";

    /* Builtin functions */
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(void)
	    ;
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char c__[12];

    /* Fortran I/O blocks */
    static icilist io___2 = { 0, c__, 0, fmt_1, 12, 1 };


    s_wsfi(&io___2);
    do_fio(&c__1, (char *)&(*l), (ftnlen)sizeof(integer));
    e_wsfi();
    s_copy(ret_val, c__, (ftnlen)12, (ftnlen)12);
    return ;
} /* cfz_ */

