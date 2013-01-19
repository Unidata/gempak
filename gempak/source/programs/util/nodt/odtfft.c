#include "math.h"
#include "inc/f2c.h"

double c_abs(complex *z);
double zabs(double real, double imag);
int fft(float *cbd,float *dx2,int *idxcnt);
int fftcc_(complex *a, integer *n, integer *iwk, float *wk);

/*
 * c_abs
 */
 
double c_abs(complex *z)
{
return( zabs( z->r, z->i ) );
}

/*
 * zabs
 */
 
double zabs(double real, double imag)
{
double temp;

if(real < 0.)
	real = -real;
if(imag < 0.)
	imag = -imag;
if(imag > real){
	temp = real;
	real = imag;
	imag = temp;
}
if((real+imag) == real)
	return(real);

temp = imag/real;
temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
return(temp);
}


int fft(float *cbd,float *dx2,int *idxcnt)
{

    static integer nbin = 64;
    /*static integer nxx = (6*64)+150;*/

    /* Local variables */
    complex chat[64];
    float magn[64],wk[534];
    integer iwk[534],nbinx=nbin;
    float a, x, dx;
    integer icbd,icnt,iok;

/*       ** perform FFT ananlysis on data */
    for (icbd = 0; icbd < nbin; ++icbd) {
	chat[icbd].r = cbd[icbd], chat[icbd].i = (float)0.;
    }
    iok=fftcc_(chat, &nbinx, iwk, wk);
    if (iok!=0) return -1;
    for (icbd = 0; icbd < nbin; ++icbd) {
	magn[icbd] = c_abs(&chat[icbd]) / (double)nbin;
    }
    dx = 0.0F;
    icnt = 0;
    for (icbd = 1; icbd < (nbin/2)-1; ++icbd) {
	a = magn[icbd-1];
	x = magn[icbd];
	dx += (x + a) / 2.0F;
	if ((magn[icbd] > magn[icbd-1])&&(magn[icbd] > magn[icbd+1])) {
	    ++icnt;
	}
    }
/* added to "normalize" fft for various satellite resolutions */
    *dx2 = dx / magn[0];
    *idxcnt=icnt;

    return 0;
} /* MAIN__ */

/* fftcc.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/
/*   IMSL ROUTINE NAME   - FFTCC */
/* ----------------------------------------------------------------------- */
/*   COMPUTER            - IBM/SINGLE */
/*   LATEST REVISION     - JANUARY 1, 1978 */
/*   PURPOSE             - COMPUTE THE FAST FOURIER TRANSFORM OF A */
/*                           COMPLEX VALUED SEQUENCE */
/*   USAGE               - CALL FFTCC (A,N,IWK,WK) */
/*   ARGUMENTS    A      - COMPLEX VECTOR OF LENGTH N. ON INPUT A */
/*                           CONTAINS THE COMPLEX VALUED SEQUENCE TO BE */
/*                           TRANSFORMED. ON OUTPUT A IS REPLACED BY THE */
/*                           FOURIER TRANSFORM. */
/*                N      - INPUT NUMBER OF DATA POINTS TO BE */
/*                           TRANSFORMED. N MAY BE ANY POSITIVE */
/*                           INTEGER. */
/*                IWK    - INTEGER WORK VECTOR OF LENGTH 6*N+150. */
/*                           (SEE PROGRAMMING NOTES FOR FURTHER DETAILS) */
/*                WK     - REAL WORK VECTOR OF LENGTH 6*N+150. */
/*                           (SEE PROGRAMMING NOTES FOR FURTHER DETAILS) */
/*   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32 */
/*                       - SINGLE/H36,H48,H60 */
/*   REQD. IMSL ROUTINES - NONE REQUIRED */
/*   NOTATION            - INFORMATION ON SPECIAL NOTATION AND */
/*                           CONVENTIONS IS AVAILABLE IN THE MANUAL */
/*                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP */
/*   REMARKS  1.  FFTCC COMPUTES THE FOURIER TRANSFORM, X, ACCORDING */
/*                TO THE FOLLOWING FORMULA; */
/*                  X(K+1) = SUM FROM J = 0 TO N-1 OF */
/*                           A(J+1)*CEXP((0.0,(2.0*PI*J*K)/N)) */
/*                  FOR K=0,1,...,N-1 AND PI=3.1415... */
/*                NOTE THAT X OVERWRITES A ON OUTPUT. */
/*            2.  FFTCC CAN BE USED TO COMPUTE */
/*                  X(K+1) = (1/N)*SUM FROM J = 0 TO N-1 OF */
/*                           A(J+1)*CEXP((0.0,(-2.0*PI*J*K)/N)) */
/*                  FOR K=0,1,...,N-1 AND PI=3.1415... */
/*                BY PERFORMING THE FOLLOWING STEPS; */
/*                     DO 10 I=1,N */
/*                        A(I) = CONJG(A(I)) */
/*                  10 CONTINUE */
/*                     CALL FFTCC (A,N,IWK,WK) */
/*                     DO 20 I=1,N */
/*                        A(I) = CONJG(A(I))/N */
/*                  20 CONTINUE */
/*   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED. */
/*   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN */
/*                           APPLIED TO THIS CODE. NO OTHER WARRANTY, */
/*                           EXPRESSED OR IMPLIED, IS APPLICABLE. */

/* ----------------------------------------------------------------------- */

/* Subroutine */ int fftcc_(complex *a, integer *n, integer *iwk, float *wk)
{
    /* Initialized data */

    static real rad = 6.283185F;
    static real c30 = 0.8660254F;
    static real zero = 0.0F;
    static real half = 0.5F;
    static real one = 1.0F;
    static real two = 2.0F;

    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1, r__2;
    complex q__1;
    static complex equiv_5[1], equiv_7[1], equiv_9[1], equiv_11[1], equiv_14[
	    1];

    /* Local variables */
    static integer i__, j, k, l, m;
#define a0 ((real *)equiv_5)
#define a1 ((real *)equiv_7)
#define a2 ((real *)equiv_9)
    static real c1, c2, c3;
#define a4 ((real *)equiv_14)
#define b4 ((real *)equiv_14 + 1)
#define a3 ((real *)equiv_11)
#define b0 ((real *)equiv_5 + 1)
    static integer k0, k1, k2, k3, l1;
#define b1 ((real *)equiv_7 + 1)
#define b2 ((real *)equiv_9 + 1)
#define b3 ((real *)equiv_11 + 1)
    static real s1, s2, s3;
#define z0 ((real *)equiv_5)
#define z1 ((real *)equiv_7)
#define z2 ((real *)equiv_9)
#define z3 ((real *)equiv_11)
#define z4 ((real *)equiv_14)
    static integer ja, ic, id, ka, kb, jf, kf, ii, kh, jj, jk, im;
    static real cm;
    static integer kn, mm, mp, kt;
    static real sm;
    static complex ak2;
    static integer kd2, mm1;
#define za0 (equiv_5)
#define za1 (equiv_7)
#define za2 (equiv_9)
#define za3 (equiv_11)
#define za4 (equiv_14)
    static integer icc, icf, ija, ikb, iam, ibm, ick, iap, ibp, ita, ird, itb,
	     ill, isf, isk, ikt, isp, iss, ktp, idm1;

/*                                  SPECIFICATIONS FOR ARGUMENTS */
/* tlo      INTEGER            N,IWK(1) */
/* tlo      REAL               WK(1) */
/*                                  SPECIFICATIONS FOR LOCAL VARIABLES */
    /* Parameter adjustments */
    --wk;
    --iwk;
    --a;

    /* Function Body */
/*                                  FIRST EXECUTABLE STATEMENT */
    if (*n == 1) {
	goto L9005;
    }
    k = *n;
    m = 0;
    j = 2;
    jj = 4;
    jf = 0;
/*                                  DETERMINE THE SQUARE FACTORS OF N */
    iwk[1] = 1;
L5:
    i__ = k / jj;
    if (i__ * jj != k) {
	goto L10;
    }
    ++m;
    iwk[m + 1] = j;
    k = i__;
    goto L5;
L10:
    j += 2;
    if (j == 4) {
	j = 3;
    }
    jj = j * j;
    if (jj <= k) {
	goto L5;
    }
    kt = m;
/*                                  DETERMINE THE REMAINING FACTORS OF N */
    j = 2;
L15:
    i__ = k / j;
    if (i__ * j != k) {
	goto L20;
    }
    ++m;
    iwk[m + 1] = j;
    k = i__;
    goto L15;
L20:
    ++j;
    if (j == 3) {
	goto L15;
    }
    ++j;
    if (j <= k) {
	goto L15;
    }
    k = iwk[m + 1];
    if (iwk[kt + 1] > iwk[m + 1]) {
	k = iwk[kt + 1];
    }
    if (kt <= 0) {
	goto L30;
    }
    ktp = kt + 2;
    i__1 = kt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	j = ktp - i__;
	++m;
	iwk[m + 1] = iwk[j];
/* L25: */
    }
L30:
    mp = m + 1;
    ic = mp + 1;
    id = ic + mp;
    ill = id + mp;
    ird = ill + mp + 1;
    icc = ird + mp;
    iss = icc + mp;
    ick = iss + mp;
    isk = ick + k;
    icf = isk + k;
    isf = icf + k;
    iap = isf + k;
    kd2 = (k - 1) / 2 + 1;
    ibp = iap + kd2;
    iam = ibp + kd2;
    ibm = iam + kd2;
    mm1 = m - 1;
    i__ = 1;
L35:
    l = mp - i__;
    j = ic - i__;
    iwk[ill + l] = 0;
    if (iwk[j - 1] + iwk[j] == 4) {
	iwk[ill + l] = 1;
    }
    if (iwk[ill + l] == 0) {
	goto L40;
    }
    ++i__;
    --l;
    iwk[ill + l] = 0;
L40:
    ++i__;
    if (i__ <= mm1) {
	goto L35;
    }
    iwk[ill + 1] = 0;
    iwk[ill + mp] = 0;
    iwk[ic] = 1;
    iwk[id] = *n;
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
	k = iwk[j + 1];
	iwk[ic + j] = iwk[ic + j - 1] * k;
	iwk[id + j] = iwk[id + j - 1] / k;
	wk[ird + j] = rad / (float)iwk[ic + j];
	c1 = rad / (float)k;
	if (k <= 2) {
	    goto L45;
	}
	wk[icc + j] = cos(c1);
	wk[iss + j] = sin(c1);
L45:
	;
    }
    mm = m;
    if (iwk[ill + m] == 1) {
	mm = m - 1;
    }
    if (mm <= 1) {
	goto L50;
    }
    sm = (float)iwk[ic + mm - 2] * wk[ird + m];
    cm = cos(sm);
    sm = sin(sm);
L50:
    kb = 0;
    kn = *n;
    jj = 0;
    i__ = 1;
    c1 = one;
    s1 = zero;
    l1 = 1;
L55:
    if (iwk[ill + i__ + 1] == 1) {
	goto L60;
    }
    kf = iwk[i__ + 1];
    goto L65;
L60:
    kf = 4;
    ++i__;
L65:
    isp = iwk[id + i__];
    if (l1 == 1) {
	goto L70;
    }
    s1 = (float)jj * wk[ird + i__];
    c1 = cos(s1);
    s1 = sin(s1);
/*                                  FACTORS OF 2, 3, AND 4 ARE */
/*                                  HANDLED SEPARATELY. */
L70:
    if (kf > 4) {
	goto L140;
    }
    switch ((int)kf) {
	case 1:  goto L75;
	case 2:  goto L75;
	case 3:  goto L90;
	case 4:  goto L115;
    }
L75:
    k0 = kb + isp;
    k2 = k0 + isp;
    if (l1 == 1) {
	goto L85;
    }
L80:
    --k0;
    if (k0 < kb) {
	goto L190;
    }
    --k2;
    i__1 = k2 + 1;
    za4->r = a[i__1].r, za4->i = a[i__1].i;
    *a0 = *a4 * c1 - *b4 * s1;
    *b0 = *a4 * s1 + *b4 * c1;
    i__1 = k2 + 1;
    i__2 = k0 + 1;
    q__1.r = a[i__2].r - za0->r, q__1.i = a[i__2].i - za0->i;
    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    i__1 = k0 + 1;
    i__2 = k0 + 1;
    q__1.r = a[i__2].r + za0->r, q__1.i = a[i__2].i + za0->i;
    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    goto L80;
L85:
    --k0;
    if (k0 < kb) {
	goto L190;
    }
    --k2;
    i__1 = k2 + 1;
    ak2.r = a[i__1].r, ak2.i = a[i__1].i;
    i__1 = k2 + 1;
    i__2 = k0 + 1;
    q__1.r = a[i__2].r - ak2.r, q__1.i = a[i__2].i - ak2.i;
    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    i__1 = k0 + 1;
    i__2 = k0 + 1;
    q__1.r = a[i__2].r + ak2.r, q__1.i = a[i__2].i + ak2.i;
    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    goto L85;
L90:
    if (l1 == 1) {
	goto L95;
    }
    c2 = c1 * c1 - s1 * s1;
    s2 = two * c1 * s1;
L95:
    ja = kb + isp - 1;
    ka = ja + kb;
    ikb = kb + 1;
    ija = ja + 1;
    i__1 = ija;
    for (ii = ikb; ii <= i__1; ++ii) {
	k0 = ka - ii + 1;
	k1 = k0 + isp;
	k2 = k1 + isp;
	i__2 = k0 + 1;
	za0->r = a[i__2].r, za0->i = a[i__2].i;
	if (l1 == 1) {
	    goto L100;
	}
	i__2 = k1 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a1 = *a4 * c1 - *b4 * s1;
	*b1 = *a4 * s1 + *b4 * c1;
	i__2 = k2 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a2 = *a4 * c2 - *b4 * s2;
	*b2 = *a4 * s2 + *b4 * c2;
	goto L105;
L100:
	i__2 = k1 + 1;
	za1->r = a[i__2].r, za1->i = a[i__2].i;
	i__2 = k2 + 1;
	za2->r = a[i__2].r, za2->i = a[i__2].i;
L105:
	i__2 = k0 + 1;
	r__1 = *a0 + *a1 + *a2;
	r__2 = *b0 + *b1 + *b2;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	*a0 = -half * (*a1 + *a2) + *a0;
	*a1 = (*a1 - *a2) * c30;
	*b0 = -half * (*b1 + *b2) + *b0;
	*b1 = (*b1 - *b2) * c30;
	i__2 = k1 + 1;
	r__1 = *a0 - *b1;
	r__2 = *b0 + *a1;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	i__2 = k2 + 1;
	r__1 = *a0 + *b1;
	r__2 = *b0 - *a1;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
/* L110: */
    }
    goto L190;
L115:
    if (l1 == 1) {
	goto L120;
    }
    c2 = c1 * c1 - s1 * s1;
    s2 = two * c1 * s1;
    c3 = c1 * c2 - s1 * s2;
    s3 = s1 * c2 + c1 * s2;
L120:
    ja = kb + isp - 1;
    ka = ja + kb;
    ikb = kb + 1;
    ija = ja + 1;
    i__1 = ija;
    for (ii = ikb; ii <= i__1; ++ii) {
	k0 = ka - ii + 1;
	k1 = k0 + isp;
	k2 = k1 + isp;
	k3 = k2 + isp;
	i__2 = k0 + 1;
	za0->r = a[i__2].r, za0->i = a[i__2].i;
	if (l1 == 1) {
	    goto L125;
	}
	i__2 = k1 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a1 = *a4 * c1 - *b4 * s1;
	*b1 = *a4 * s1 + *b4 * c1;
	i__2 = k2 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a2 = *a4 * c2 - *b4 * s2;
	*b2 = *a4 * s2 + *b4 * c2;
	i__2 = k3 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a3 = *a4 * c3 - *b4 * s3;
	*b3 = *a4 * s3 + *b4 * c3;
	goto L130;
L125:
	i__2 = k1 + 1;
	za1->r = a[i__2].r, za1->i = a[i__2].i;
	i__2 = k2 + 1;
	za2->r = a[i__2].r, za2->i = a[i__2].i;
	i__2 = k3 + 1;
	za3->r = a[i__2].r, za3->i = a[i__2].i;
L130:
	i__2 = k0 + 1;
	r__1 = *a0 + *a2 + *a1 + *a3;
	r__2 = *b0 + *b2 + *b1 + *b3;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	i__2 = k1 + 1;
	r__1 = *a0 + *a2 - *a1 - *a3;
	r__2 = *b0 + *b2 - *b1 - *b3;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	i__2 = k2 + 1;
	r__1 = *a0 - *a2 - *b1 + *b3;
	r__2 = *b0 - *b2 + *a1 - *a3;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	i__2 = k3 + 1;
	r__1 = *a0 - *a2 + *b1 - *b3;
	r__2 = *b0 - *b2 - *a1 + *a3;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
/* L135: */
    }
    goto L190;
L140:
    jk = kf - 1;
    kh = jk / 2;
    k3 = iwk[id + i__ - 1];
    k0 = kb + isp;
    if (l1 == 1) {
	goto L150;
    }
    k = jk - 1;
    wk[icf + 1] = c1;
    wk[isf + 1] = s1;
    i__1 = k;
    for (j = 1; j <= i__1; ++j) {
	wk[icf + j + 1] = wk[icf + j] * c1 - wk[isf + j] * s1;
	wk[isf + j + 1] = wk[icf + j] * s1 + wk[isf + j] * c1;
/* L145: */
    }
L150:
    if (kf == jf) {
	goto L160;
    }
    c2 = wk[icc + i__];
    wk[ick + 1] = c2;
    wk[ick + jk] = c2;
    s2 = wk[iss + i__];
    wk[isk + 1] = s2;
    wk[isk + jk] = -s2;
    i__1 = kh;
    for (j = 1; j <= i__1; ++j) {
	k = jk - j;
	wk[ick + k] = wk[ick + j] * c2 - wk[isk + j] * s2;
	wk[ick + j + 1] = wk[ick + k];
	wk[isk + j + 1] = wk[ick + j] * s2 + wk[isk + j] * c2;
	wk[isk + k] = -wk[isk + j + 1];
/* L155: */
    }
L160:
    --k0;
    k1 = k0;
    k2 = k0 + k3;
    i__1 = k0 + 1;
    za0->r = a[i__1].r, za0->i = a[i__1].i;
    *a3 = *a0;
    *b3 = *b0;
    i__1 = kh;
    for (j = 1; j <= i__1; ++j) {
	k1 += isp;
	k2 -= isp;
	if (l1 == 1) {
	    goto L165;
	}
	k = kf - j;
	i__2 = k1 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a1 = *a4 * wk[icf + j] - *b4 * wk[isf + j];
	*b1 = *a4 * wk[isf + j] + *b4 * wk[icf + j];
	i__2 = k2 + 1;
	za4->r = a[i__2].r, za4->i = a[i__2].i;
	*a2 = *a4 * wk[icf + k] - *b4 * wk[isf + k];
	*b2 = *a4 * wk[isf + k] + *b4 * wk[icf + k];
	goto L170;
L165:
	i__2 = k1 + 1;
	za1->r = a[i__2].r, za1->i = a[i__2].i;
	i__2 = k2 + 1;
	za2->r = a[i__2].r, za2->i = a[i__2].i;
L170:
	wk[iap + j] = *a1 + *a2;
	wk[iam + j] = *a1 - *a2;
	wk[ibp + j] = *b1 + *b2;
	wk[ibm + j] = *b1 - *b2;
	*a3 = *a1 + *a2 + *a3;
	*b3 = *b1 + *b2 + *b3;
/* L175: */
    }
    i__1 = k0 + 1;
    q__1.r = *a3, q__1.i = *b3;
    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    k1 = k0;
    k2 = k0 + k3;
    i__1 = kh;
    for (j = 1; j <= i__1; ++j) {
	k1 += isp;
	k2 -= isp;
	jk = j;
	*a1 = *a0;
	*b1 = *b0;
	*a2 = zero;
	*b2 = zero;
	i__2 = kh;
	for (k = 1; k <= i__2; ++k) {
	    *a1 += wk[iap + k] * wk[ick + jk];
	    *a2 += wk[iam + k] * wk[isk + jk];
	    *b1 += wk[ibp + k] * wk[ick + jk];
	    *b2 += wk[ibm + k] * wk[isk + jk];
	    jk += j;
	    if (jk >= kf) {
		jk -= kf;
	    }
/* L180: */
	}
	i__2 = k1 + 1;
	r__1 = *a1 - *b2;
	r__2 = *b1 + *a2;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	i__2 = k2 + 1;
	r__1 = *a1 + *b2;
	r__2 = *b1 - *a2;
	q__1.r = r__1, q__1.i = r__2;
	a[i__2].r = q__1.r, a[i__2].i = q__1.i;
/* L185: */
    }
    if (k0 > kb) {
	goto L160;
    }
    jf = kf;
L190:
    if (i__ >= mm) {
	goto L195;
    }
    ++i__;
    goto L55;
L195:
    i__ = mm;
    l1 = 0;
    kb = iwk[id + i__ - 1] + kb;
    if (kb >= kn) {
	goto L215;
    }
L200:
    jj = iwk[ic + i__ - 2] + jj;
    if (jj < iwk[ic + i__ - 1]) {
	goto L205;
    }
    --i__;
    jj -= iwk[ic + i__];
    goto L200;
L205:
    if (i__ != mm) {
	goto L210;
    }
    c2 = c1;
    c1 = cm * c1 - sm * s1;
    s1 = sm * c2 + cm * s1;
    goto L70;
L210:
    if (iwk[ill + i__] == 1) {
	++i__;
    }
    goto L55;
L215:
    i__ = 1;
    ja = kt - 1;
    ka = ja + 1;
    if (ja < 1) {
	goto L225;
    }
    i__1 = ja;
    for (ii = 1; ii <= i__1; ++ii) {
	j = ka - ii;
	--iwk[j + 1];
	i__ = iwk[j + 1] + i__;
/* L220: */
    }
/*                                  THE RESULT IS NOW PERMUTED TO */
/*                                  NORMAL ORDER. */
L225:
    if (kt <= 0) {
	goto L270;
    }
    j = 1;
    i__ = 0;
    kb = 0;
L230:
    k2 = iwk[id + j] + kb;
    k3 = k2;
    jj = iwk[ic + j - 1];
    jk = jj;
    k0 = kb + jj;
    isp = iwk[ic + j] - jj;
L235:
    k = k0 + jj;
L240:
    i__1 = k0 + 1;
    za4->r = a[i__1].r, za4->i = a[i__1].i;
    i__1 = k0 + 1;
    i__2 = k2 + 1;
    a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
    i__1 = k2 + 1;
    a[i__1].r = za4->r, a[i__1].i = za4->i;
    ++k0;
    ++k2;
    if (k0 < k) {
	goto L240;
    }
    k0 += isp;
    k2 += isp;
    if (k0 < k3) {
	goto L235;
    }
    if (k0 >= k3 + isp) {
	goto L245;
    }
    k0 = k0 - iwk[id + j] + jj;
    goto L235;
L245:
    k3 = iwk[id + j] + k3;
    if (k3 - kb >= iwk[id + j - 1]) {
	goto L250;
    }
    k2 = k3 + jk;
    jk += jj;
    k0 = k3 - iwk[id + j] + jk;
    goto L235;
L250:
    if (j >= kt) {
	goto L260;
    }
    k = iwk[j + 1] + i__;
    ++j;
L255:
    ++i__;
    iwk[ill + i__] = j;
    if (i__ < k) {
	goto L255;
    }
    goto L230;
L260:
    kb = k3;
    if (i__ <= 0) {
	goto L265;
    }
    j = iwk[ill + i__];
    --i__;
    goto L230;
L265:
    if (kb >= *n) {
	goto L270;
    }
    j = 1;
    goto L230;
L270:
    jk = iwk[ic + kt];
    isp = iwk[id + kt];
    m -= kt;
    kb = isp / jk - 2;
    if (kt >= m - 1) {
	goto L9005;
    }
    ita = ill + kb + 1;
    itb = ita + jk;
    idm1 = id - 1;
    ikt = kt + 1;
    im = m + 1;
    i__1 = im;
    for (j = ikt; j <= i__1; ++j) {
	iwk[idm1 + j] /= jk;
/* L275: */
    }
    jj = 0;
    i__1 = kb;
    for (j = 1; j <= i__1; ++j) {
	k = kt;
L280:
	jj = iwk[id + k + 1] + jj;
	if (jj < iwk[id + k]) {
	    goto L285;
	}
	jj -= iwk[id + k];
	++k;
	goto L280;
L285:
	iwk[ill + j] = jj;
	if (jj == j) {
	    iwk[ill + j] = -j;
	}
/* L290: */
    }
/*                                  DETERMINE THE PERMUTATION CYCLES */
/*                                  OF LENGTH GREATER THAN OR EQUAL */
/*                                  TO TWO. */
    i__1 = kb;
    for (j = 1; j <= i__1; ++j) {
	if (iwk[ill + j] <= 0) {
	    goto L300;
	}
	k2 = j;
L295:
	k2 = (i__2 = iwk[ill + k2], abs(i__2));
	if (k2 == j) {
	    goto L300;
	}
	iwk[ill + k2] = -iwk[ill + k2];
	goto L295;
L300:
	;
    }
/*                                  REORDER A FOLLOWING THE */
/*                                  PERMUTATION CYCLES */
    i__ = 0;
    j = 0;
    kb = 0;
    kn = *n;
L305:
    ++j;
    if (iwk[ill + j] < 0) {
	goto L305;
    }
    k = iwk[ill + j];
    k0 = jk * k + kb;
L310:
    i__1 = k0 + i__ + 1;
    za4->r = a[i__1].r, za4->i = a[i__1].i;
    wk[ita + i__] = *a4;
    wk[itb + i__] = *b4;
    ++i__;
    if (i__ < jk) {
	goto L310;
    }
    i__ = 0;
L315:
    k = -iwk[ill + k];
    jj = k0;
    k0 = jk * k + kb;
L320:
    i__1 = jj + i__ + 1;
    i__2 = k0 + i__ + 1;
    a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
    ++i__;
    if (i__ < jk) {
	goto L320;
    }
    i__ = 0;
    if (k != j) {
	goto L315;
    }
L325:
    i__1 = k0 + i__ + 1;
    i__2 = ita + i__;
    i__3 = itb + i__;
    q__1.r = wk[i__2], q__1.i = wk[i__3];
    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    ++i__;
    if (i__ < jk) {
	goto L325;
    }
    i__ = 0;
    if (j < k2) {
	goto L305;
    }
    j = 0;
    kb += isp;
    if (kb < kn) {
	goto L305;
    }
L9005:
    return 0;
} /* fftcc_ */

#undef za4
#undef za3
#undef za2
#undef za1
#undef za0
#undef z4
#undef z3
#undef z2
#undef z1
#undef z0
#undef b3
#undef b2
#undef b1
#undef b0
#undef a3
#undef b4
#undef a4
#undef a2
#undef a1
#undef a0


