/* lit.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: lit.f,v 1.1 2000/07/12 13:12:26 gad Exp $ *** */
/* $ Name: */
/* $      lit     - Returns the four bytes of a character string as an integer. */
/* $ */
/* $ Interface: */
/* $	character*4 function */
/* $      lit(integer c) */
/* $ */
/* $ Input: */
/* $      c       - Character string that is copied to an integer. */
/* $ */
/* $ Input and Output: */
/* $	none */
/* $ */
/* $ Output: */
/* $	none */
/* $ */
/* $ Return values: */
/* $      The four bytes of the character string. */
/* $ */
/* $ Remarks: */
/* $      This function is designed to move the four bytes of a character string */
/* $      to an integer variable.  It enables programmers to avoid conversions */
/* $      implicit in replacement statements in which variables of different */
/* $      types are on opposite sides of the equal sign. */
/* $ */
/* $ Categories: */
/* $      converter */
integer lit_(char *c__, ftnlen c_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char c1[4];
    static integer l1;
    extern /* Subroutine */ int movcw_(char *, integer *, ftnlen);

    s_copy(c1, c__, (ftnlen)4, c_len);
    movcw_(c1, &l1, (ftnlen)4);
    ret_val = l1;
    return ret_val;
} /* lit_ */

