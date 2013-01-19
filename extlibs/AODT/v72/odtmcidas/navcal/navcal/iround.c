/* iround.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
integer iround_(real *x)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer i_nint(real *);

/* *** $Id: iround.f,v 1.1 2001/04/16 20:59:07 daves Exp $ *** */
/* $ FUNCTION IROUND(X)  (JMB) */
/* $ ROUNDS A FLOATING POINT VALUE */
/* $ X = (R) INPUT  FLOATING POINT VALUE */
/* $$ IROUND = REAL */

    ret_val = i_nint(x);
    return ret_val;
} /* iround_ */

