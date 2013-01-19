/* fltcon2.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"
#include "mcidas.h"
#include "mcsubs.h"

/* Table of constant values */

static int c__1 = 1;
static float c_b4 = (float)16.;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* Subroutine */ int fltcon_(array, num)
float *array;
int *num;
{
    /* System generated locals */
    int i__1;
    static float equiv_0[1];

    /* Builtin functions 
    double pow_ri(); */

    /* Local variables */
    static float frac;
    static int jexp;
#define local ((int *)equiv_0)
    static float fsign;
    static int kk;
#define flocal (equiv_0)
    static float val;

/* *** $Id: fltcon.c,v 1.4 2000/08/18 13:11:06 gad Exp $ *** */

/*  THIS SUBROUTINE WILL CONVERT AN ARRAY OF MVS FLOATING POINT NUMBERS */
/*  INTO PC/RISC FROMAT FLOATING POINT NUMBERS. */

/*     ARRAY: REAL ARRAY OF NUMBERS TO BE CONVERTED (INPUT/OUTPUT) */
/*     NUM: DIMENSION OF ARRAY (INPUT) */

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: fltcon.c,v 1.4 2000/08/18 13:11:06 gad Exp $ *** */

/* --- LOOP THROUGH THE INPUT ARRAY */

    /* Parameter adjustments */
    --array;

    /* Function Body */
    i__1 = *num;
    for (kk = 1; kk <= i__1; ++kk) {
	*flocal = array[kk];

/* --- FLIP THE BYTES FOR A PC (SWBYT4 DOES NOTHING ON A RISC) */
	swbyt4_(flocal, &c__1);
	if (*local == 0) {
	    array[kk] = (float)0.;
	} else if (*local == -2139062144) {
	    goto L100;
	} else {
	    fsign = *flocal;
	    *flocal = dabs(*flocal);

/* --- 16777216 = 2**24; 70 = OFFSET FOR RIGHT JUSTIFICATION */

	    jexp = *local / 16777216 - 70;
	    frac = (float) (*local % 16777216);
	    val = frac * pow_ri(&c_b4, &jexp);
	    if (fsign < (float)0.) {
		val = -val;
	    }
	    array[kk] = val;

	}

L100:
	;
    }

    return 0;
} /* fltcon_ */

#undef flocal
#undef local


