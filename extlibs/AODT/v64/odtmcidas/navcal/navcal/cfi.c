/* cfi.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: cfi.f,v 1.9 1997/10/10 20:16:40 dglo Exp $ *** */
/* $ CFI(I)  (RJL) */
/* $ CONVERT INTEGER TO CHARACTER*12. RETURNED VALUE IS DIGITS RIGHT- */
/* $   JUSTIFIED WITH LEADING BLANKS. */
/* $ I = (I) INPUT  VALUE TO BE CONVERTED */
/* $$ CFI = CONVERT, INTEGER, CHARACTER */
/* -----NOTE: (THIS ROUTINE DESIGNED FOR MACHINES WITH 36- OR FEWER */
/*        BIT WORD.) */

/* $ Name: */
/* $      cfi     - Converts an integer to a character*12, right */
/* $                justified, with leading blanks. */
/* $ */
/* $ Interface: */
/* $      character*12 function */
/* $      cfi(integer i) */
/* $ */
/* $ Input: */
/* $      i       - Number to be converted. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      none */
/* $ */
/* $ Return values: */
/* $      Converted number. */
/* $ */
/* $ Remarks: */
/* $      none */
/* $ */
/* $ Categories: */
/* $      text */
/* $      converter */
/* Character */ VOID cfi_(ret_val, ret_val_len, i__)
char *ret_val;
ftnlen ret_val_len;
integer *i__;
{
    /* Builtin functions */
    /* Subroutine */ int s_copy();

    /* Local variables */
    static integer k, m, izero;



    izero = '0';
    m = abs(*i__);
/* -----K ALWAYS POINTS AT THE NEXT POSITION IN THE OUTPUT FIELD, */
/* -----   STARTING FROM THE RIGHT. */
    for (k = 12; k >= 2; --k) {
	*(unsigned char *)&ret_val[k - 1] = (char) (m % 10 + izero);
	m /= 10;
	if (m <= 0) {
	    goto L3;
	}
/* L2: */
    }
    k = 2;
/* -----CHECK FOR MINUS SIGN NECESSARY */
L3:
    if (*i__ < 0) {
	--k;
	*(unsigned char *)&ret_val[k - 1] = '-';
    }
/* -----INSERT LEADING BLANKS */
    if (k > 1) {
	s_copy(ret_val, " ", k - 1, (ftnlen)1);
    }
    return ;
} /* cfi_ */

