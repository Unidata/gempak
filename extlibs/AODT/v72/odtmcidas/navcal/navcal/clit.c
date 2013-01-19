/* clit.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: clit.f,v 1.1 2000/06/26 15:51:47 daves Exp $ *** */
/* $ Name: */
/* $      clit    - Returns the four bytes of an integer as a character string. */
/* $ */
/* $ Interface: */
/* $	character*4 function */
/* $	clit(integer l) */
/* $ */
/* $ Input: */
/* $      l       - Integer that is copied to a character variable. */
/* $ */
/* $ Input and Output: */
/* $	none */
/* $ */
/* $ Output: */
/* $	none */
/* $ */
/* $ Return values: */
/* $      The four bytes of the input integer. */
/* $ */
/* $ Remarks: */
/* $      This function is designed to move the four bytes of an integer to a */
/* $      character variable.  It enables programmers to avoid conversions */
/* $      implicit in replacement statements in which variables of different */
/* $      types are on opposite sides of the equal sign. */
/* $ */
/* $ Categories: */
/* $      converter */
/* Character */ VOID clit_(char *ret_val, ftnlen ret_val_len, integer *l)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char c__[4];
    extern /* Subroutine */ int movwc_(integer *, char *, ftnlen);

/* --- local variables */
    movwc_(l, c__, (ftnlen)4);
    s_copy(ret_val, c__, (ftnlen)4, (ftnlen)4);
    return ;
} /* clit_ */

