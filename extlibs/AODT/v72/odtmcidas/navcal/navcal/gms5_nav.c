/* gms5_nav.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal dtims;
    real reslin[4], reselm[4], rlic[4], relmfc[4], senssu[4], rline[4], 
	    relmnt[4], vmis[3], elmis[9]	/* was [3][3] */;
    doublereal dspin, orbt1[280]	/* was [35][8] */, atit[100]	/* 
	    was [10][10] */;
} mmap1_;

#define mmap1_1 mmap1_

struct {
    real sublat, sublon;
} nav1_;

#define nav1_1 nav1_

/* Table of constant values */

static doublereal c_b2 = 10.;
static integer c__1 = 1;
static integer c__6 = 6;
static integer c__8 = 8;
static integer c__4 = 4;
static integer c__10 = 10;
static integer c__0 = 0;
static integer c__7 = 7;
static integer c__11 = 11;
static integer c__12 = 12;
static integer c__14 = 14;
static integer c__16 = 16;
static doublereal c_b180 = 360.;
static real c_b193 = 10.f;
static integer c__5 = 5;
static integer c__3 = 3;

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: gms5_nav.f,v 1.1 2000/06/26 15:51:47 daves Exp $ *** */
/* $ Name: */
/* $      gms5_nav - Collection of subsidary routines required by */
/* $                 gms5.pgm and nvxgmsx.dlm/dll. */
/* $ */
/* Subroutine */ int sv0100_(integer *iword, integer *ipos, char *c__, real *
	r4dat, doublereal *r8dat, ftnlen c_len)
{
    /* Builtin functions */
    double pow_di(doublereal *, integer *);

    /* Local variables */
    static integer idata1;

/* ************************************************************ */
/*     TYPE CONVERT ROUTINE ( R-TYPE ) */
/* ************************************************************ */


/* ============================================================= */

    /* Parameter adjustments */
    --c__;

    /* Function Body */
    *r4dat = 0.f;
    *r8dat = 0.;
    if (*iword == 4) {
	idata1 = *(unsigned char *)&c__[1] / 128;
	*r8dat = (doublereal) (*(unsigned char *)&c__[1] % 128) * 16777216. + 
		(doublereal) (*(unsigned char *)&c__[2]) * 65536. + (
		doublereal) (*(unsigned char *)&c__[3]) * 256. + (doublereal) 
		(*(unsigned char *)&c__[4]);
	*r8dat /= pow_di(&c_b2, ipos);
	if (idata1 == 1) {
	    *r8dat = -(*r8dat);
	}
	*r4dat = (real) (*r8dat);
    } else if (*iword == 6) {
	idata1 = *(unsigned char *)&c__[1] / 128;
	*r8dat = (doublereal) (*(unsigned char *)&c__[1] % 128) * 
		1099511627776. + (doublereal) (*(unsigned char *)&c__[2]) * 
		4294967296. + (doublereal) (*(unsigned char *)&c__[3]) * 
		16777216. + (doublereal) (*(unsigned char *)&c__[4]) * 65536. 
		+ (doublereal) (*(unsigned char *)&c__[5]) * 256. + (
		doublereal) (*(unsigned char *)&c__[6]);
	*r8dat /= pow_di(&c_b2, ipos);
	if (idata1 == 1) {
	    *r8dat = -(*r8dat);
	}
	*r4dat = (real) (*r8dat);
    }
    return 0;
} /* sv0100_ */

/* Subroutine */ int sv0110_(integer *iword, char *c__, integer *i4dat, 
	ftnlen c_len)
{
/* ****************************************************************** */
/*     TYPE CONVERT ROUTINE ( I-TYPE ) */
/* ****************************************************************** */


/* ==================================================================== */

    /* Parameter adjustments */
    --c__;

    /* Function Body */
    *i4dat = 0;
    if (*iword == 2) {
	*i4dat = (*(unsigned char *)&c__[1] << 8) + *(unsigned char *)&c__[2];
    } else if (*iword == 4) {
	*i4dat = (*(unsigned char *)&c__[1] << 24) + (*(unsigned char *)&c__[
		2] << 16) + (*(unsigned char *)&c__[3] << 8) + *(unsigned 
		char *)&c__[4];
    }
    return 0;
} /* sv0110_ */

/* Subroutine */ int sv0200_(char *csmt, integer *ismt, ftnlen csmt_len)
{
    static integer il1, il2, il3, ilat, ilon, iline1, ipixe1;

/* ********************************************************************* */
/*     SIMPLIFIED MAPPING DATA PROCESSING ROUTINE */
/* ********************************************************************* */


/* ====================================================================== */

    /* Parameter adjustments */
    ismt -= 651;
    --csmt;

    /* Function Body */
    for (il1 = 1; il1 <= 25; ++il1) {
	for (il2 = 1; il2 <= 25; ++il2) {
	    ilat = 60 - (il1 - 1) * 5;
	    ilon = (il2 - 1) * 5 + 80;
	    il3 = (il1 - 1) * 100 + (il2 - 1 << 2) + 1;
	    iline1 = (*(unsigned char *)&csmt[il3] << 8) + *(unsigned char *)&
		    csmt[il3 + 1];
	    ipixe1 = (*(unsigned char *)&csmt[il3 + 2] << 8) + *(unsigned 
		    char *)&csmt[il3 + 3];
	    ismt[il2 + (il1 + 25) * 25] = ilat;
	    ismt[il2 + (il1 + 50) * 25] = ilon;
	    ismt[il2 + (il1 + 75) * 25] = iline1;
	    ismt[il2 + (il1 + 100) * 25] = ipixe1;
/* L2200: */
	}
/* L2100: */
    }
    return 0;
} /* sv0200_ */

/* Subroutine */ int sv0400_(real *rdl, real *rdp)
{
    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);
    double cos(doublereal), sin(doublereal);
    integer i_nint(real *);

    /* Local variables */
    static real ca, cb, cc, sa, sb, sc;
    static integer il1, il2, ismt[2500]	/* was [25][25][4] */;

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 6, 0, "(A16,F12.8)", 0 };
    static cilist io___10 = { 0, 6, 0, "(A16,F12.8)", 0 };


/* ----------------------------------------------------------------- */
/*     MAPPING TABLE CORRECTION ROUTINE                           C */
/* ----------------------------------------------------------------- */

/*                                     *CORRECT ORBIT/ATTITUDE DATA */
    mmap1_1.vmis[1] -= *rdl * mmap1_1.reslin[1];
    mmap1_1.vmis[2] += *rdp * mmap1_1.reselm[1];
    s_wsfe(&io___9);
    do_fio(&c__1, " CORRECT Y-MIS :", (ftnlen)16);
    do_fio(&c__1, (char *)&mmap1_1.vmis[1], (ftnlen)sizeof(real));
    e_wsfe();
    s_wsfe(&io___10);
    do_fio(&c__1, " CORRECT Z-MIS :", (ftnlen)16);
    do_fio(&c__1, (char *)&mmap1_1.vmis[2], (ftnlen)sizeof(real));
    e_wsfe();
    ca = cos(mmap1_1.vmis[0]);
    cb = cos(mmap1_1.vmis[1]);
    cc = cos(mmap1_1.vmis[2]);
    sa = sin(mmap1_1.vmis[0]);
    sb = sin(mmap1_1.vmis[1]);
    sc = sin(mmap1_1.vmis[2]);
    mmap1_1.elmis[0] = cc * cb;
    mmap1_1.elmis[1] = -sc * cb;
    mmap1_1.elmis[2] = sb;
    mmap1_1.elmis[3] = cc * sb * sa + sc * ca;
    mmap1_1.elmis[4] = -sc * sb * sa + cc * ca;
    mmap1_1.elmis[5] = -cb * sa;
    mmap1_1.elmis[6] = cc * sb * ca + sc * sa;
    mmap1_1.elmis[7] = -sc * sb * ca + cc * sa;
    mmap1_1.elmis[8] = cb * ca;
/*                                   *CORRECT MAPPING TABLE */
    for (il1 = 1; il1 <= 25; ++il1) {
	for (il2 = 1; il2 <= 25; ++il2) {
	    ismt[il2 + (il1 + 75) * 25 - 651] += i_nint(rdl);
	    ismt[il2 + (il1 + 100) * 25 - 651] += i_nint(rdp);
/* L1100: */
	}
/* L1000: */
    }

    return 0;
} /* sv0400_ */

/* Subroutine */ int decode_oa_block__(char *cobat, char *form, ftnlen 
	cobat_len, ftnlen form_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int sv0100_(integer *, integer *, char *, real *, 
	    doublereal *, ftnlen);
    static real r4dmy;
    static doublereal r8dmy;

/* ************************************************************************ */
/*     ORBIT AND ATTITUDE DATA PROCESSING ROUTINE */
/* ************************************************************************ */




/* ========================================================================= */


/* Initialize Values in common block */
    mmap1_1.dtims = 0.f;
    mmap1_1.dspin = 0.f;
    for (i__ = 1; i__ <= 4; ++i__) {
	mmap1_1.reslin[i__ - 1] = 0.f;
	mmap1_1.reselm[i__ - 1] = 0.f;
	mmap1_1.rlic[i__ - 1] = 0.f;
	mmap1_1.relmfc[i__ - 1] = 0.f;
	mmap1_1.senssu[i__ - 1] = 0.f;
	mmap1_1.rline[i__ - 1] = 0.f;
	mmap1_1.relmnt[i__ - 1] = 0.f;
/* L1000: */
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	mmap1_1.vmis[i__ - 1] = 0.f;
	for (j = 1; j <= 3; ++j) {
	    mmap1_1.elmis[i__ + j * 3 - 4] = 0.f;
/* L1200: */
	}
/* L1100: */
    }
    for (i__ = 1; i__ <= 10; ++i__) {
	for (j = 1; j <= 10; ++j) {
	    mmap1_1.atit[i__ + j * 10 - 11] = 0.f;
/* L1400: */
	}
/* L1300: */
    }
    for (i__ = 1; i__ <= 35; ++i__) {
	for (j = 1; j <= 8; ++j) {
	    mmap1_1.orbt1[i__ + j * 35 - 36] = 0.f;
/* L1600: */
	}
/* L1500: */
    }
    sv0100_(&c__6, &c__8, cobat, &r4dmy, &mmap1_1.dtims, (ftnlen)6);
    sv0100_(&c__4, &c__8, cobat + 6, mmap1_1.reslin, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__8, cobat + 10, &mmap1_1.reslin[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__8, cobat + 10, &mmap1_1.reslin[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__8, cobat + 10, &mmap1_1.reslin[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 14, mmap1_1.reselm, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 18, &mmap1_1.reselm[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 18, &mmap1_1.reselm[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 18, &mmap1_1.reselm[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 22, mmap1_1.rlic, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 26, &mmap1_1.rlic[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 110, &mmap1_1.rlic[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 114, &mmap1_1.rlic[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 30, mmap1_1.relmfc, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 34, &mmap1_1.relmfc[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 118, &mmap1_1.relmfc[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__4, cobat + 122, &mmap1_1.relmfc[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 38, mmap1_1.senssu, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 42, &mmap1_1.senssu[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 42, &mmap1_1.senssu[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 42, &mmap1_1.senssu[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 46, mmap1_1.rline, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 50, &mmap1_1.rline[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 50, &mmap1_1.rline[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 50, &mmap1_1.rline[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 54, mmap1_1.relmnt, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 58, &mmap1_1.relmnt[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 58, &mmap1_1.relmnt[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__0, cobat + 58, &mmap1_1.relmnt[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 62, mmap1_1.vmis, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 66, &mmap1_1.vmis[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 70, &mmap1_1.vmis[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__7, cobat + 74, mmap1_1.elmis, &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 78, &mmap1_1.elmis[1], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 82, &mmap1_1.elmis[2], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 86, &mmap1_1.elmis[3], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__7, cobat + 90, &mmap1_1.elmis[4], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 94, &mmap1_1.elmis[5], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 98, &mmap1_1.elmis[6], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__10, cobat + 102, &mmap1_1.elmis[7], &r8dmy, (ftnlen)4);
    sv0100_(&c__4, &c__7, cobat + 106, &mmap1_1.elmis[8], &r8dmy, (ftnlen)4);
    sv0100_(&c__6, &c__8, cobat + 240, &r4dmy, &mmap1_1.dspin, (ftnlen)6);
    sv0100_(&c__6, &c__6, cobat + 198, &nav1_1.sublon, &r8dmy, (ftnlen)6);
    sv0100_(&c__6, &c__6, cobat + 204, &nav1_1.sublat, &r8dmy, (ftnlen)6);

    for (i__ = 1; i__ <= 10; ++i__) {
	if (s_cmp(form, "long ", (ftnlen)5, (ftnlen)5) == 0) {
	    j = (i__ - 1 << 6) + 256;
	}
	if (s_cmp(form, "short", (ftnlen)5, (ftnlen)5) == 0) {
	    j = (i__ - 1) * 48 + 256;
	}
	i__1 = j;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.atit[i__ * 10 - 
		10], j + 6 - i__1);
	i__1 = j + 12;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.atit[i__ * 10 - 
		8], j + 18 - i__1);
	i__1 = j + 18;
	sv0100_(&c__6, &c__11, cobat + i__1, &r4dmy, &mmap1_1.atit[i__ * 10 - 
		7], j + 24 - i__1);
	i__1 = j + 24;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.atit[i__ * 10 - 
		6], j + 30 - i__1);
	i__1 = j + 30;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.atit[i__ * 10 - 
		5], j + 36 - i__1);
/* L2000: */
    }

    for (i__ = 1; i__ <= 8; ++i__) {
	if (s_cmp(form, "long ", (ftnlen)5, (ftnlen)5) == 0) {
	    j = (i__ - 1 << 8) + 896;
	}
	if (s_cmp(form, "short", (ftnlen)5, (ftnlen)5) == 0) {
	    j = (i__ - 1) * 200 + 736;
	}
	i__1 = j;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		35], j + 6 - i__1);
	i__1 = j + 48;
	sv0100_(&c__6, &c__6, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		27], j + 54 - i__1);
	i__1 = j + 54;
	sv0100_(&c__6, &c__6, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		26], j + 60 - i__1);
	i__1 = j + 60;
	sv0100_(&c__6, &c__6, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		25], j + 66 - i__1);
	i__1 = j + 84;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		21], j + 90 - i__1);
	i__1 = j + 102;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		18], j + 108 - i__1);
	i__1 = j + 108;
	sv0100_(&c__6, &c__8, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 - 
		17], j + 114 - i__1);
	i__1 = j + 128;
	sv0100_(&c__6, &c__12, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 16], j + 134 - i__1);
	i__1 = j + 134;
	sv0100_(&c__6, &c__14, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 15], j + 140 - i__1);
	i__1 = j + 140;
	sv0100_(&c__6, &c__14, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 14], j + 146 - i__1);
	i__1 = j + 146;
	sv0100_(&c__6, &c__14, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 13], j + 152 - i__1);
	i__1 = j + 152;
	sv0100_(&c__6, &c__12, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 12], j + 158 - i__1);
	i__1 = j + 158;
	sv0100_(&c__6, &c__16, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 11], j + 164 - i__1);
	i__1 = j + 164;
	sv0100_(&c__6, &c__12, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 10], j + 170 - i__1);
	i__1 = j + 170;
	sv0100_(&c__6, &c__16, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 9], j + 176 - i__1);
	i__1 = j + 176;
	sv0100_(&c__6, &c__12, cobat + i__1, &r4dmy, &mmap1_1.orbt1[i__ * 35 
		- 8], j + 182 - i__1);
/* L3000: */
    }
    return 0;
} /* decode_oa_block__ */

/* Subroutine */ int encode_oa_block__(char *cobat, ftnlen cobat_len)
{
    extern integer encode_real4__(real *, integer *, char *, ftnlen);
    static integer iret;

/* ************************************************************************ */
/* ************************************************************************ */

    iret = encode_real4__(mmap1_1.vmis, &c__10, cobat + 62, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.vmis[1], &c__10, cobat + 66, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.vmis[2], &c__10, cobat + 70, (ftnlen)4);
    iret = encode_real4__(mmap1_1.elmis, &c__7, cobat + 74, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[1], &c__10, cobat + 78, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[2], &c__10, cobat + 82, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[3], &c__10, cobat + 86, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[4], &c__7, cobat + 90, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[5], &c__10, cobat + 94, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[6], &c__10, cobat + 98, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[7], &c__10, cobat + 102, (ftnlen)4);
    iret = encode_real4__(&mmap1_1.elmis[8], &c__7, cobat + 106, (ftnlen)4);
    return 0;
} /* encode_oa_block__ */

/* Subroutine */ int sublatlon_(real *lonlat)
{
    /* Parameter adjustments */
    --lonlat;

    /* Function Body */
    lonlat[1] = nav1_1.sublat;
    lonlat[2] = nav1_1.sublon * -1.f;
    return 0;
} /* sublatlon_ */

/* Subroutine */ int mgivsr_(integer *imode, real *rpix, real *rlin, real *
	rlon, real *rlat, real *rhgt, real *rinf, doublereal *dsct, integer *
	irtn)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sin(doublereal), sqrt(doublereal), cos(doublereal), atan(
	    doublereal), tan(doublereal), r_int(real *), d_mod(doublereal *, 
	    doublereal *);

    /* Local variables */
    static doublereal bc, ea, dd, ee, ef, dk, en, pc, bs, qc, pi, tf;
    static real ri, rj;
    static doublereal sl[3], tl, ps, qs, tp, sp[3], ss[3], sx[3], sy[3], dk1, 
	    dk2, sw1[3], sw2[3], sw3[3], dda, ddb, ddc, def, cdr, crd;
    static real eps;
    static doublereal sat[3];
    static real rio;
    static doublereal slv[3], stn1[3], stn2[3], stn3[3];
    extern /* Subroutine */ int mg1100_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), mg1200_(
	    doublereal *, doublereal *), mg1220_(doublereal *, doublereal *, 
	    doublereal *), mg1210_(doublereal *, doublereal *, doublereal *), 
	    mg1230_(doublereal *, doublereal *, doublereal *);
    static doublereal beta, dpai;
    extern /* Subroutine */ int mg1240_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal hpai, dlat;
    static real rfcl, rfcp;
    static doublereal dlon, sdis;
    static real rftl, sens, rftp;
    static doublereal rtim, sunm, dsata, dssda, dsatd;
    static integer lmode;
    static doublereal dlatn, dsuna, dlonn, dsung;
    static real rsamp;
    static doublereal dsatz, wkcos, wksin;
    static real rstep;
    static doublereal dsunz;

/* ************************************************************** */
/* ************************************************************** */
/* ************************************************************** */
/* ************************************************************** */
/* ************************************************************** */

/*  THIS PROGRAM CONVERTS GEOGRAPHICAL CO-ORDINATES (LAT,LONG, */
/*  HEIGHT) TO VISSR IMAGE CO-ORDINATES (LINE,PIXEL) AND VICE */
/*  VERSA. */

/*  THIS PROGRAM IS PROVIDED BY THE METEOROLOGICAL SATELLITE */
/*  CENTRE OF THE JAPAN METEOROLOGICAL AGENCY TO USERS OF GMS */
/*  DATA. */

/*                               MSC TECH. NOTE NO.23 */
/*                                         JMA/MSC 1991 */

/* ************************************************************** */
/* ************************************************************** */
/* ************************************************************** */
/* ************************************************************** */
/*           I/O  TYPE */
/*  IMODE     I   I*4   CONVERSION MODE & IMAGE KIND */
/*                       IMAGE KIND */
/*                             GMS-4 GMS-5 */
/*                        1,-1  VIS   VIS */
/*                        2,-2  IR    IR1 */
/*                        3,-3  --    IR2 */
/*                        4,-4  --    WV */
/*                      CONVERSION MODE */
/*                        1 TO  4   (LAT,LON,HGT)=>(LINE,PIXEL) */
/*                       -1 TO -4   (LAT,LON    )<=(LINE,PIXEL) */
/*  RPIX    I/O  R*4    PIXEL OF POINT */
/*  RLIN    I/O  R*4    LINE OF POINT */
/*  RLON    I/0  R*4    LONG. OF POINT (DEGREES,EAST:+,WEST:-) */
/*  RLAT    I/O  R*4    LAT.  OF POINT (DEGREES,NORTH:+,SOURTH:-) */
/*  RHGT     I   R*4    HEIGHT OF POINT (METER) */
/*  RINF(8)  O   R*4    (1) SATELLITE ZENITH DISTANCE (DEGREES) */
/*                      (2) SATELLITE AZIMUTH ANGLE (DEGREES) */
/*                      (3) SUN ZENITH DISTANCE (DEGREES) */
/*                      (4) SUN AZIMUTH ANGLE (DEGREES) */
/*                      (5) SATELLITE-SUN DEPARTURE ANGLE (DEG) */
/*                      (6) SATELLITE DISTANCE (METER) */
/*                      (7) SUN DISTANCE (KILOMETER) */
/*                      (8) SUN GLINT ANLGLE (DEGREES) */
/*  DSCT     O  R*8     SCAN TIME (MJD) */
/*  IRTN     O  I*4     RETURN CODE (0 = OK) */

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */


/*   1. COORDINATE TRANSFORMATION PARAMETERS SEGMENT */
/*                                              MAP(1,1)-MAP(672,1) */
/*   2. ATTITUDE PREDICTION DATA SEGMENT        MAP(1,2)-MAP(672,2) */
/*   3. ORBIT PREDICTION DATA 1 SEGMENT         MAP(1,3)-MAP(672,3) */
/*   4. ORBIT PREDICTION DATA 2 SEGMENT         MAP(1,4)-MAP(672,4) */
/* ***************************************************************** */

/* !!!!!!!!!!!!!!!! DEFINITION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */






/* ================================================================== */

    /* Parameter adjustments */
    --rinf;

    /* Function Body */
    pi = 3.141592653;
    cdr = pi / 180.;
    crd = 180. / pi;
    hpai = pi / 2.;
    dpai = pi * 2.;
    ea = 6378136.;
    ef = .0033528131778969143;
    eps = 1.f;
/* !!!!!!!!!!!!!!!!!!!!!!PARAMETER CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    *irtn = 0;
    if (abs(*imode) > 4) {
	*irtn = 1;
    }
    if (abs(*rlat) > 90.f && *imode > 0) {
	*irtn = 2;
    }
    if (*irtn != 0) {
	return 0;
    }
/* !!!!!!!!!!!!!!!!!!!!!!VISSR FRAME INFORMATION SET!!!!!!!!!!!!!!!!!!!!!! */
    lmode = abs(*imode);
    rstep = mmap1_1.reslin[lmode - 1];
    rsamp = mmap1_1.reselm[lmode - 1];
    rfcl = mmap1_1.rlic[lmode - 1];
    rfcp = mmap1_1.relmfc[lmode - 1];
    sens = mmap1_1.senssu[lmode - 1];
    rftl = mmap1_1.rline[lmode - 1] + .5f;
    rftp = mmap1_1.relmnt[lmode - 1] + .5f;
/* !!!!!!!!!!!!!!!!!!!!!TRANSFORMATION (GEOGRAPHICAL=>VISSR)!!!!!!!!!!!!!!! */
    if (*imode > 0 && *imode < 5) {
	dlat = (doublereal) (*rlat) * cdr;
	dlon = (doublereal) (*rlon) * cdr;
	ee = ef * 2. - ef * ef;
	en = ea / sqrt(1. - ee * sin(dlat) * sin(dlat));
	stn1[0] = (en + (doublereal) (*rhgt)) * cos(dlat) * cos(dlon);
	stn1[1] = (en + (doublereal) (*rhgt)) * cos(dlat) * sin(dlon);
	stn1[2] = (en * (1. - ee) + (doublereal) (*rhgt)) * sin(dlat);

	rio = rfcl - atan(sin((real) dlat) / (6.610689f - cos((real) dlat))) /
		 rstep;
	rtim = mmap1_1.dtims + (doublereal) (rio / sens / 1440.f) / 
		mmap1_1.dspin;

L100:
	mg1100_(&rtim, &cdr, sat, sp, ss, &beta);
/* ----------------------------------------------------------------------- */
	mg1220_(sp, ss, sw1);
	mg1220_(sw1, sp, sw2);
	bc = cos(beta);
	bs = sin(beta);
	sw3[0] = sw1[0] * bs + sw2[0] * bc;
	sw3[1] = sw1[1] * bs + sw2[1] * bc;
	sw3[2] = sw1[2] * bs + sw2[2] * bc;
	mg1200_(sw3, sx);
	mg1220_(sp, sx, sy);
	slv[0] = stn1[0] - sat[0];
	slv[1] = stn1[1] - sat[1];
	slv[2] = stn1[2] - sat[2];
	mg1200_(slv, sl);
	mg1210_(sp, sl, sw2);
	mg1210_(sy, sw2, sw3);
	mg1230_(sy, sw2, &tp);
	tf = sp[0] * sw3[0] + sp[1] * sw3[1] + sp[2] * sw3[2];
	if (tf < 0.) {
	    tp = -tp;
	}
	mg1230_(sp, sl, &tl);

	ri = (real) (hpai - tl) / rstep + rfcl - mmap1_1.vmis[1] / rstep;
	rj = (real) tp / rsamp + rfcp + mmap1_1.vmis[2] / rsamp - (real) (
		hpai - tl) * tan(mmap1_1.vmis[0]) / rsamp;

	if ((r__1 = ri - rio, abs(r__1)) >= eps) {
	    r__1 = (ri - 1.f) / sens;
	    rtim = (doublereal) (r_int(&r__1) + rj * rsamp / (real) dpai) / (
		    mmap1_1.dspin * 1440.) + mmap1_1.dtims;
	    rio = ri;
	    goto L100;
	}
	*rlin = ri;
	*rpix = rj;
	*dsct = rtim;
	if (*rlin < 0.f || *rlin > rftl) {
	    *irtn = 4;
	}
	if (*rpix < 0.f || *rpix > rftp) {
	    *irtn = 5;
	}

/* !!!!!!!!!!!!!!!!!TRANSFORMATION (VISSR=>GEOGRAPHICAL)!!!!!!!!!!!!!!!!!! */
    } else if (*imode < 0 && *imode > -5) {
	r__1 = (*rlin - 1.f) / sens;
	rtim = (doublereal) (r_int(&r__1) + *rpix * rsamp / (real) dpai) / (
		mmap1_1.dspin * 1440.) + mmap1_1.dtims;
	mg1100_(&rtim, &cdr, sat, sp, ss, &beta);
	mg1220_(sp, ss, sw1);
	mg1220_(sw1, sp, sw2);
	bc = cos(beta);
	bs = sin(beta);
	sw3[0] = sw1[0] * bs + sw2[0] * bc;
	sw3[1] = sw1[1] * bs + sw2[1] * bc;
	sw3[2] = sw1[2] * bs + sw2[2] * bc;
	mg1200_(sw3, sx);
	mg1220_(sp, sx, sy);
	pc = cos((doublereal) (rstep * (*rlin - rfcl)));
	ps = sin((doublereal) (rstep * (*rlin - rfcl)));
	qc = cos((doublereal) (rsamp * (*rpix - rfcp)));
	qs = sin((doublereal) (rsamp * (*rpix - rfcp)));
	sw1[0] = (doublereal) mmap1_1.elmis[0] * pc + (doublereal) 
		mmap1_1.elmis[6] * ps;
	sw1[1] = (doublereal) mmap1_1.elmis[1] * pc + (doublereal) 
		mmap1_1.elmis[7] * ps;
	sw1[2] = (doublereal) mmap1_1.elmis[2] * pc + (doublereal) 
		mmap1_1.elmis[8] * ps;
	sw2[0] = qc * sw1[0] - qs * sw1[1];
	sw2[1] = qs * sw1[0] + qc * sw1[1];
	sw2[2] = sw1[2];
	sw3[0] = sx[0] * sw2[0] + sy[0] * sw2[1] + sp[0] * sw2[2];
	sw3[1] = sx[1] * sw2[0] + sy[1] * sw2[1] + sp[1] * sw2[2];
	sw3[2] = sx[2] * sw2[0] + sy[2] * sw2[1] + sp[2] * sw2[2];
	mg1200_(sw3, sl);
	def = (1. - ef) * (1. - ef);
	dda = def * (sl[0] * sl[0] + sl[1] * sl[1]) + sl[2] * sl[2];
	ddb = def * (sat[0] * sl[0] + sat[1] * sl[1]) + sat[2] * sl[2];
	ddc = def * (sat[0] * sat[0] + sat[1] * sat[1] - ea * ea) + sat[2] * 
		sat[2];
	dd = ddb * ddb - dda * ddc;
	if (dd >= 0. && dda != 0.) {
	    dk1 = (-ddb + sqrt(dd)) / dda;
	    dk2 = (-ddb - sqrt(dd)) / dda;
	} else {
	    *irtn = 6;
	    goto L9000;
	}
	if (abs(dk1) <= abs(dk2)) {
	    dk = dk1;
	} else {
	    dk = dk2;
	}
	stn1[0] = sat[0] + dk * sl[0];
	stn1[1] = sat[1] + dk * sl[1];
	stn1[2] = sat[2] + dk * sl[2];
	dlat = atan(stn1[2] / (def * sqrt(stn1[0] * stn1[0] + stn1[1] * stn1[
		1])));
	if (stn1[0] != 0.) {
	    dlon = atan(stn1[1] / stn1[0]);
	    if (stn1[0] < 0. && stn1[1] >= 0.) {
		dlon += pi;
	    }
	    if (stn1[0] < 0. && stn1[1] < 0.) {
		dlon -= pi;
	    }
	} else {
	    if (stn1[1] > 0.) {
		dlon = hpai;
	    } else {
		dlon = -hpai;
	    }
	}
	*rlat = (real) (dlat * crd);
	*rlon = (real) (dlon * crd);
	*dsct = rtim;
    }

/* !!!!!!!!!!!!!!!!!!!TRANSFORMATION (ZENITH/AZIMUTH)!!!!!!!!!!!!!!! */
    stn2[0] = cos(dlat) * cos(dlon);
    stn2[1] = cos(dlat) * sin(dlon);
    stn2[2] = sin(dlat);
    slv[0] = sat[0] - stn1[0];
    slv[1] = sat[1] - stn1[1];
    slv[2] = sat[2] - stn1[2];
    mg1200_(slv, sl);

    mg1230_(stn2, sl, &dsatz);
    if (dsatz > hpai) {
	*irtn = 7;
    }
/*     write(6,7011)dsatz,hpai */
/* 7011 format(' irtn=7,dsatz,hpai:',2f10.4) */

    sunm = rtim * .9856 + 315.253;
    sunm = d_mod(&sunm, &c_b180) * cdr;
    sdis = (1.0014 - cos(sunm) * .01672 - cos(sunm * 2.) * 1.4e-4f) * 
	    149597870.;

    if (dlat >= 0.) {
	dlatn = hpai - dlat;
	dlonn = dlon - pi;
	if (dlonn <= -pi) {
	    dlonn += dpai;
	}
    } else {
	dlatn = hpai + dlat;
	dlonn = dlon;
    }
    stn3[0] = cos(dlatn) * cos(dlonn);
    stn3[1] = cos(dlatn) * sin(dlonn);
    stn3[2] = sin(dlatn);
    sw1[0] = slv[0] + ss[0] * sdis * 1e3;
    sw1[1] = slv[1] + ss[1] * sdis * 1e3;
    sw1[2] = slv[2] + ss[2] * sdis * 1e3;
    mg1200_(sw1, sw2);
    mg1230_(stn2, sw2, &dsunz);
    mg1230_(sl, sw2, &dssda);
    mg1240_(sl, stn2, stn3, &dpai, &dsata);
    mg1240_(sw2, stn2, stn3, &dpai, &dsuna);
    dsatd = sqrt(slv[0] * slv[0] + slv[1] * slv[1] + slv[2] * slv[2]);


    mg1200_(stn1, sl);
    mg1230_(sw2, sl, &dsung);
    mg1220_(sl, sw2, sw3);
    mg1220_(sw3, sl, sw1);
    wkcos = cos(dsung);
    wksin = sin(dsung);
    sw2[0] = wkcos * sl[0] - wksin * sw1[0];
    sw2[1] = wkcos * sl[1] - wksin * sw1[1];
    sw2[2] = wkcos * sl[2] - wksin * sw1[2];
    mg1230_(sw2, slv, &dsung);

    rinf[6] = (real) dsatd;
    rinf[7] = (real) sdis;
    rinf[1] = (real) (dsatz * crd);
    rinf[2] = (real) (dsata * crd);
    rinf[3] = (real) (dsunz * crd);
    rinf[4] = (real) (dsuna * crd);
    rinf[5] = (real) (dssda * crd);
    rinf[8] = (real) (dsung * crd);
/* !!!!!!!!!!!!!!!!!!!!!!!!!STOP/END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
L9000:
    return 0;
} /* mgivsr_ */

/* Subroutine */ int mg1100_(doublereal *rtim, doublereal *cdr, doublereal *
	sat, doublereal *sp, doublereal *ss, doublereal *beta)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    static integer i__;
    static doublereal npa[9]	/* was [3][3] */, att1[3], att2[3], att3[3];
    extern /* Subroutine */ int mg1110_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), mg1200_(doublereal *, 
	    doublereal *);
    static doublereal delt, orbt2[280]	/* was [35][8] */, wkcos, wksin, 
	    attdel, attalp, sundel, sitagt, sunalp;



    /* Parameter adjustments */
    --ss;
    --sp;
    --sat;

    /* Function Body */
    for (i__ = 1; i__ <= 7; ++i__) {
	if (*rtim >= mmap1_1.orbt1[i__ * 35 - 35] && *rtim < mmap1_1.orbt1[(
		i__ + 1) * 35 - 35]) {
	    mg1110_(&i__, rtim, cdr, mmap1_1.orbt1, orbt2, &sat[1], &sitagt, &
		    sunalp, &sundel, npa);
	    goto L1200;
	}
/* L1000: */
    }
L1200:

    for (i__ = 1; i__ <= 9; ++i__) {
	if (*rtim >= mmap1_1.atit[i__ * 10 - 10] && *rtim < mmap1_1.atit[(i__ 
		+ 1) * 10 - 10]) {
	    delt = (*rtim - mmap1_1.atit[i__ * 10 - 10]) / (mmap1_1.atit[(i__ 
		    + 1) * 10 - 10] - mmap1_1.atit[i__ * 10 - 10]);
	    attalp = mmap1_1.atit[i__ * 10 - 8] + (mmap1_1.atit[(i__ + 1) * 
		    10 - 8] - mmap1_1.atit[i__ * 10 - 8]) * delt;
	    attdel = mmap1_1.atit[i__ * 10 - 7] + (mmap1_1.atit[(i__ + 1) * 
		    10 - 7] - mmap1_1.atit[i__ * 10 - 7]) * delt;
	    *beta = mmap1_1.atit[i__ * 10 - 6] + (mmap1_1.atit[(i__ + 1) * 10 
		    - 6] - mmap1_1.atit[i__ * 10 - 6]) * delt;
	    if (mmap1_1.atit[(i__ + 1) * 10 - 6] - mmap1_1.atit[i__ * 10 - 6] 
		    > 0.) {
		*beta = mmap1_1.atit[i__ * 10 - 6] + (mmap1_1.atit[(i__ + 1) *
			 10 - 6] - mmap1_1.atit[i__ * 10 - 6] - *cdr * 360.) *
			 delt;
	    }
	    goto L3001;
	}
/* L3000: */
    }
L3001:

    wkcos = cos(attdel);
    att1[0] = sin(attdel);
    att1[1] = wkcos * (-sin(attalp));
    att1[2] = wkcos * cos(attalp);
    att2[0] = npa[0] * att1[0] + npa[3] * att1[1] + npa[6] * att1[2];
    att2[1] = npa[1] * att1[0] + npa[4] * att1[1] + npa[7] * att1[2];
    att2[2] = npa[2] * att1[0] + npa[5] * att1[1] + npa[8] * att1[2];
    wksin = sin(sitagt);
    wkcos = cos(sitagt);
    att3[0] = wkcos * att2[0] + wksin * att2[1];
    att3[1] = -wksin * att2[0] + wkcos * att2[1];
    att3[2] = att2[2];
    mg1200_(att3, &sp[1]);

    wkcos = cos(sundel);
    ss[1] = wkcos * cos(sunalp);
    ss[2] = wkcos * sin(sunalp);
    ss[3] = sin(sundel);

    return 0;
} /* mg1100_ */

/* Subroutine */ int mg1110_(integer *i__, doublereal *rtim, doublereal *cdr, 
	doublereal *orbta, doublereal *orbtb, doublereal *sat, doublereal *
	sitagt, doublereal *sunalp, doublereal *sundel, doublereal *npa)
{
    static doublereal delt;



    /* Parameter adjustments */
    npa -= 4;
    --sat;
    orbtb -= 36;
    orbta -= 36;

    /* Function Body */
    if (*i__ != 8) {
	delt = (*rtim - orbta[*i__ * 35 + 1]) / (orbta[(*i__ + 1) * 35 + 1] - 
		orbta[*i__ * 35 + 1]);
	sat[1] = orbta[*i__ * 35 + 9] + (orbta[(*i__ + 1) * 35 + 9] - orbta[*
		i__ * 35 + 9]) * delt;
	sat[2] = orbta[*i__ * 35 + 10] + (orbta[(*i__ + 1) * 35 + 10] - orbta[
		*i__ * 35 + 10]) * delt;
	sat[3] = orbta[*i__ * 35 + 11] + (orbta[(*i__ + 1) * 35 + 11] - orbta[
		*i__ * 35 + 11]) * delt;
	*sitagt = (orbta[*i__ * 35 + 15] + (orbta[(*i__ + 1) * 35 + 15] - 
		orbta[*i__ * 35 + 15]) * delt) * *cdr;
	if (orbta[(*i__ + 1) * 35 + 15] - orbta[*i__ * 35 + 15] < 0.) {
	    *sitagt = (orbta[*i__ * 35 + 15] + (orbta[(*i__ + 1) * 35 + 15] - 
		    orbta[*i__ * 35 + 15] + 360.) * delt) * *cdr;
	}
	*sunalp = (orbta[*i__ * 35 + 18] + (orbta[(*i__ + 1) * 35 + 18] - 
		orbta[*i__ * 35 + 18]) * delt) * *cdr;
	if (orbta[(*i__ + 1) * 35 + 18] - orbta[*i__ * 35 + 18] > 0.) {
	    *sunalp = (orbta[*i__ * 35 + 18] + (orbta[(*i__ + 1) * 35 + 18] - 
		    orbta[*i__ * 35 + 18] - 360.) * delt) * *cdr;
	}
	*sundel = (orbta[*i__ * 35 + 19] + (orbta[(*i__ + 1) * 35 + 19] - 
		orbta[*i__ * 35 + 19]) * delt) * *cdr;
	npa[4] = orbta[*i__ * 35 + 20];
	npa[5] = orbta[*i__ * 35 + 21];
	npa[6] = orbta[*i__ * 35 + 22];
	npa[7] = orbta[*i__ * 35 + 23];
	npa[8] = orbta[*i__ * 35 + 24];
	npa[9] = orbta[*i__ * 35 + 25];
	npa[10] = orbta[*i__ * 35 + 26];
	npa[11] = orbta[*i__ * 35 + 27];
	npa[12] = orbta[*i__ * 35 + 28];
    }
    return 0;
} /* mg1110_ */

/* Subroutine */ int mg1200_(doublereal *vect, doublereal *vectu)
{
    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal rv1, rv2;

    /* Parameter adjustments */
    --vectu;
    --vect;

    /* Function Body */
    rv1 = vect[1] * vect[1] + vect[2] * vect[2] + vect[3] * vect[3];
    if (rv1 == 0.) {
	return 0;
    }
    rv2 = sqrt(rv1);
    vectu[1] = vect[1] / rv2;
    vectu[2] = vect[2] / rv2;
    vectu[3] = vect[3] / rv2;
    return 0;
} /* mg1200_ */

/* Subroutine */ int mg1210_(doublereal *va, doublereal *vb, doublereal *vc)
{
    /* Parameter adjustments */
    --vc;
    --vb;
    --va;

    /* Function Body */
    vc[1] = va[2] * vb[3] - va[3] * vb[2];
    vc[2] = va[3] * vb[1] - va[1] * vb[3];
    vc[3] = va[1] * vb[2] - va[2] * vb[1];
    return 0;
} /* mg1210_ */

/* Subroutine */ int mg1220_(doublereal *va, doublereal *vb, doublereal *vd)
{
    static doublereal vc[3];
    extern /* Subroutine */ int mg1200_(doublereal *, doublereal *);

    /* Parameter adjustments */
    --vd;
    --vb;
    --va;

    /* Function Body */
    vc[0] = va[2] * vb[3] - va[3] * vb[2];
    vc[1] = va[3] * vb[1] - va[1] * vb[3];
    vc[2] = va[1] * vb[2] - va[2] * vb[1];
    mg1200_(vc, &vd[1]);
    return 0;
} /* mg1220_ */

/* Subroutine */ int mg1230_(doublereal *va, doublereal *vb, doublereal *
	asita)
{
    /* Builtin functions */
    double sqrt(doublereal), acos(doublereal);

    /* Local variables */
    static doublereal as1, as2;

    /* Parameter adjustments */
    --vb;
    --va;

    /* Function Body */
    as1 = va[1] * vb[1] + va[2] * vb[2] + va[3] * vb[3];
    as2 = (va[1] * va[1] + va[2] * va[2] + va[3] * va[3]) * (vb[1] * vb[1] + 
	    vb[2] * vb[2] + vb[3] * vb[3]);
    if (as2 == 0.) {
	return 0;
    }
    *asita = acos(as1 / sqrt(as2));
    return 0;
} /* mg1230_ */

/* Subroutine */ int mg1240_(doublereal *va, doublereal *vh, doublereal *vn, 
	doublereal *dpai, doublereal *azi)
{
    static doublereal vb[3], vc[3], vd[3];
    extern /* Subroutine */ int mg1220_(doublereal *, doublereal *, 
	    doublereal *), mg1230_(doublereal *, doublereal *, doublereal *);
    static doublereal dnai;

    /* Parameter adjustments */
    --vn;
    --vh;
    --va;

    /* Function Body */
    mg1220_(&vn[1], &vh[1], vb);
    mg1220_(&va[1], &vh[1], vc);
    mg1230_(vb, vc, azi);
    mg1220_(vb, vc, vd);
    dnai = vd[0] * vh[1] + vd[1] * vh[2] + vd[2] * vh[3];
    if (dnai > 0.) {
	*azi = *dpai - *azi;
    }
    return 0;
} /* mg1240_ */

integer encode_real4__(real *r4dat, integer *ipos, char *c4, ftnlen c4_len)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Builtin functions */
    double pow_ri(real *, integer *);
    integer pow_ii(integer *, integer *);

    /* Local variables */
    static integer i__, base, idat, tint, nflag, iword;

    /* Parameter adjustments */
    --c4;

    /* Function Body */
    base = 256;
    nflag = 0;
    iword = 4;
    if (*r4dat < 0.f) {
	nflag = 1;
    }
    idat = (i__1 = (integer) (*r4dat * pow_ri(&c_b193, ipos)), abs(i__1));
    i__1 = iword;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = iword - i__;
	tint = idat / pow_ii(&base, &i__2);
	if (i__ == 1) {
	    if (tint > 127) {
		ret_val = -1;
		goto L999;
	    } else if (tint >= 1) {
		i__2 = iword - i__;
		idat -= tint * pow_ii(&base, &i__2);
	    }
	    if (nflag == 0) {
		*(unsigned char *)&c4[i__] = (char) tint;
	    }
	    if (nflag == 1) {
		*(unsigned char *)&c4[i__] = (char) (tint + 128);
	    }
	} else {
	    if (tint >= 1) {
		i__2 = iword - i__;
		idat -= tint * pow_ii(&base, &i__2);
		*(unsigned char *)&c4[i__] = (char) tint;
	    } else {
		*(unsigned char *)&c4[i__] = '\0';
	    }
	}
/* L100: */
    }
    ret_val = 0;
L999:
    return ret_val;
} /* encode_real4__ */

integer encode_real8__(doublereal *r8dat, integer *ipos, char *c6, ftnlen 
	c6_len)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double pow_di(doublereal *, integer *);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer i__;
    static doublereal base;
    static integer tint, nflag;
    extern doublereal floor_(doublereal *);
    static integer iword;

    /* Fortran I/O blocks */
    static cilist io___128 = { 0, 6, 0, 0, 0 };


    /* Parameter adjustments */
    --c6;

    /* Function Body */
    base = 256.;
    nflag = 0;
    iword = 6;
    if (*r8dat < 0.) {
	nflag = 1;
    }
    *r8dat = (d__1 = *r8dat * pow_di(&c_b2, ipos), abs(d__1));
    i__1 = iword;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = iword - i__;
	tint = (integer) (*r8dat / pow_di(&base, &i__2));
	s_wsle(&io___128);
	do_lio(&c__5, &c__1, (char *)&(*r8dat), (ftnlen)sizeof(doublereal));
	do_lio(&c__3, &c__1, (char *)&tint, (ftnlen)sizeof(integer));
	e_wsle();
	if (i__ == 1) {
	    if (tint > 127) {
		ret_val = -1;
		goto L999;
	    } else if (tint >= 1) {
		i__2 = iword - i__;
		*r8dat -= tint * 1. * pow_di(&base, &i__2);
		d__1 = *r8dat + .5;
		*r8dat = floor_(&d__1);
	    }
	    if (nflag == 0) {
		*(unsigned char *)&c6[i__] = (char) tint;
	    }
	    if (nflag == 1) {
		*(unsigned char *)&c6[i__] = (char) (tint + 128);
	    }
	} else {
	    if (tint >= 1) {
		i__2 = iword - i__;
		*r8dat -= tint * 1. * pow_di(&base, &i__2);
		d__1 = *r8dat + .5;
		*r8dat = floor_(&d__1);
		*(unsigned char *)&c6[i__] = (char) tint;
	    } else {
		*(unsigned char *)&c6[i__] = '\0';
	    }
	}
/* L100: */
    }
    ret_val = 0;
L999:
    return ret_val;
} /* encode_real8__ */

doublereal floor_(doublereal *x)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double d_int(doublereal *);

/*     COMPUTES "FLOOR" FUNCTION */
    if (*x >= 0.) {
	ret_val = d_int(x);
    } else {
	ret_val = d_int(x) - 1.;
    }
    return ret_val;
} /* floor_ */

