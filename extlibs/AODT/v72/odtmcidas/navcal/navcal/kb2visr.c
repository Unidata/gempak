/* kb2visr.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer jtype, isou, ides, jopt[5];
} visrxxvisrkb2_;

#define visrxxvisrkb2_1 visrxxvisrkb2_

struct {
    char caltyp[4];
} brkpntvisrkb2_;

#define brkpntvisrkb2_1 brkpntvisrkb2_

/* Table of constant values */

static integer c__5 = 5;
static integer c__1 = 1;
static integer c__2 = 2;

integer kb2inivisr_(cin, cout, iopt, cin_len, cout_len)
char *cin, *cout;
integer *iopt;
ftnlen cin_len;
ftnlen cout_len;
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp();

    /* Local variables */
    extern /* Subroutine */ int movw_();

/* symbolic constants & shared data */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.11 1998/03/12 20:11:17 rickk Rel $ *** */
/*  area subsystem parameters */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/* NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  IF YOU CHANGE THESE VALUES, YOU MUST ALSO CHANGE THEM IN */
/*   MCIDAS.H !! */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  MAXGRIDPT		maximum number of grid points */
/*  MAX_BANDS		maximum number of bands within an area */

/*  MAXDFELEMENTS	maximum number of elements that DF can handle */
/* 			in an area line */
/*  MAXOPENAREAS		maximum number of areas that the library can */
/* 			have open (formerly called `NA') */
/*  NUMAREAOPTIONS	number of options settable through ARAOPT() */
/* 			It is presently 5 because there are five options */
/* 			that ARAOPT() knows about: */
/* 				'PREC','SPAC','UNIT','SCAL','CALB' */
/* 			(formerly called `NB') */
/* --- Size (number of words) in an area directory */
/* 	MAX_AUXBLOCK_SIZE	size (in bytes) of the internal buffers */
/* 				used to recieve AUX blocks during an */
/* 				ADDE transaction */

/* ----- MAX_AREA_NUMBER        Maximum area number allowed on system */

/* external functions */
/* local variables */
    /* Parameter adjustments */
    --iopt;

    /* Function Body */
    movw_(&c__5, &iopt[1], visrxxvisrkb2_1.jopt);
    visrxxvisrkb2_1.jtype = 0;
    visrxxvisrkb2_1.isou = iopt[1];
    visrxxvisrkb2_1.ides = iopt[2];
    if (s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "TEMP", (
	    ftnlen)4, (ftnlen)4) == 0) {
	visrxxvisrkb2_1.jtype = 1;
    }
    if (s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "MODB", (
	    ftnlen)4, (ftnlen)4) == 0) {
	visrxxvisrkb2_1.jtype = 2;
    }
    if (s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "RAW ", (
	    ftnlen)4, (ftnlen)4) == 0) {
	visrxxvisrkb2_1.jtype = 3;
    }
    if (visrxxvisrkb2_1.jtype == 0) {
	goto L900;
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* kb2inivisr_ */

integer kb2calvisr_(calb, idir, nval, iband, ibuf)
integer *calb, *idir, *nval, *iband, *ibuf;
{
    /* Initialized data */

    static integer lasday = -1;
    static integer lastyp = -1;

    /* System generated locals */
    integer ret_val;
    real r__1;
    char ch__1[4];

    /* Builtin functions */
    integer s_cmp();

    /* Local variables */
    static integer itab[256], iday;
    extern /* Character */ VOID clit_();
    static integer iret;
    extern logical isir_();
    static integer idir4, i__;
    extern /* Subroutine */ int temptbvisrkb2_();
    extern integer brkval_();
    /*extern / Subroutine / int mpixel_(), mpixtb_();*/
    extern /* Subroutine */ int mpixel (), mpixtb_();
    extern integer mciydtocyd_();

/* symbolic constants & shared data */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.11 1998/03/12 20:11:17 rickk Rel $ *** */
/*  area subsystem parameters */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/* NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  IF YOU CHANGE THESE VALUES, YOU MUST ALSO CHANGE THEM IN */
/*   MCIDAS.H !! */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  MAXGRIDPT		maximum number of grid points */
/*  MAX_BANDS		maximum number of bands within an area */

/*  MAXDFELEMENTS	maximum number of elements that DF can handle */
/* 			in an area line */
/*  MAXOPENAREAS		maximum number of areas that the library can */
/* 			have open (formerly called `NA') */
/*  NUMAREAOPTIONS	number of options settable through ARAOPT() */
/* 			It is presently 5 because there are five options */
/* 			that ARAOPT() knows about: */
/* 				'PREC','SPAC','UNIT','SCAL','CALB' */
/* 			(formerly called `NB') */
/* --- Size (number of words) in an area directory */
/* 	MAX_AUXBLOCK_SIZE	size (in bytes) of the internal buffers */
/* 				used to recieve AUX blocks during an */
/* 				ADDE transaction */

/* ----- MAX_AREA_NUMBER        Maximum area number allowed on system */

/* external functions */
/* local variables */
/* initialized variables */
    /* Parameter adjustments */
    --ibuf;
    --idir;
    --calb;

    /* Function Body */
    if (idir[4] > 1900000) {
	idir4 = idir[4];
    } else {
	iret = mciydtocyd_(&idir[4], &idir4);
    }
    clit_(ch__1, (ftnlen)4, &idir[57]);
    if (! isir_(&idir[3], ch__1, iband, (ftnlen)4)) {
	if ((s_cmp(brkpntvisrkb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 0 
		|| s_cmp(brkpntvisrkb2_1.caltyp, "RAW ", (ftnlen)4, (ftnlen)4)
		 == 0) && visrxxvisrkb2_1.jtype == 2) {
	    if (lasday != idir4 || lastyp != visrxxvisrkb2_1.jtype) {
		for (i__ = 0; i__ <= 255; ++i__) {
/* L100: */
		    r__1 = (real) i__;
		    itab[i__] = brkval_(&r__1);
		}
		lasday = idir4;
		lastyp = visrxxvisrkb2_1.jtype;
	    }
	    mpixtb_(nval, &visrxxvisrkb2_1.isou, &visrxxvisrkb2_1.ides, &ibuf[
		    1], itab);
	    return ret_val;
	} else {
	    mpixel (nval, &visrxxvisrkb2_1.isou, &visrxxvisrkb2_1.ides, &ibuf[
		    1]);
	    return ret_val;
	}
    }
    iday = idir4;
    if (lasday != idir4 || lastyp != visrxxvisrkb2_1.jtype) {
	if (visrxxvisrkb2_1.jtype == 1) {
	    temptbvisrkb2_(&iday, itab);
	} else if (visrxxvisrkb2_1.jtype == 3) {
	    for (i__ = 0; i__ <= 255; ++i__) {
/* L105: */
		itab[i__] = i__;
	    }
	} else if (s_cmp(brkpntvisrkb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4)
		 == 0 || s_cmp(brkpntvisrkb2_1.caltyp, "RAW", (ftnlen)4, (
		ftnlen)3) == 0) {
	    for (i__ = 0; i__ <= 255; ++i__) {
/* L110: */
		r__1 = (real) i__;
		itab[i__] = brkval_(&r__1);
	    }
	} else if (s_cmp(brkpntvisrkb2_1.caltyp, "TEMP", (ftnlen)4, (ftnlen)4)
		 == 0) {
	    temptbvisrkb2_(&iday, itab);
	    for (i__ = 0; i__ <= 255; ++i__) {
		r__1 = (real) itab[i__] / (float)10.;
		itab[i__] = brkval_(&r__1);
/* L200: */
	    }
	}
	lasday = idir4;
	lastyp = visrxxvisrkb2_1.jtype;
    }
    mpixtb_(nval, &visrxxvisrkb2_1.isou, &visrxxvisrkb2_1.ides, &ibuf[1], 
	    itab);
    ret_val = 0;
    return ret_val;
} /* kb2calvisr_ */

integer kb2optvisr_(cfunc, iin, iout, cfunc_len)
char *cfunc;
integer *iin, *iout;
ftnlen cfunc_len;
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    integer s_cmp();

    /* Local variables */
    extern /* Character */ VOID clit_();
    extern logical isir_();
    static char cfile[8];
    extern /* Subroutine */ int movcw_();
    extern integer ischar_(), brkset_(), lit_();

/* symbolic constants & shared data */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.11 1998/03/12 20:11:17 rickk Rel $ *** */
/*  area subsystem parameters */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/* NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  IF YOU CHANGE THESE VALUES, YOU MUST ALSO CHANGE THEM IN */
/*   MCIDAS.H !! */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  MAXGRIDPT		maximum number of grid points */
/*  MAX_BANDS		maximum number of bands within an area */

/*  MAXDFELEMENTS	maximum number of elements that DF can handle */
/* 			in an area line */
/*  MAXOPENAREAS		maximum number of areas that the library can */
/* 			have open (formerly called `NA') */
/*  NUMAREAOPTIONS	number of options settable through ARAOPT() */
/* 			It is presently 5 because there are five options */
/* 			that ARAOPT() knows about: */
/* 				'PREC','SPAC','UNIT','SCAL','CALB' */
/* 			(formerly called `NB') */
/* --- Size (number of words) in an area directory */
/* 	MAX_AUXBLOCK_SIZE	size (in bytes) of the internal buffers */
/* 				used to recieve AUX blocks during an */
/* 				ADDE transaction */

/* ----- MAX_AREA_NUMBER        Maximum area number allowed on system */

/* external functions */
/* local variables */
    /* Parameter adjustments */
    --iout;
    --iin;

    /* Function Body */
    if (s_cmp(cfunc, "KEYS", (ftnlen)4, (ftnlen)4) == 0) {
	clit_(ch__1, (ftnlen)4, &iin[37]);
	if (! isir_(&iin[1], ch__1, &iin[4], (ftnlen)4)) {
	    iout[1] = 2;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("BRIT", (ftnlen)4);
	} else {
	    iout[1] = 3;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("TEMP", (ftnlen)4);
	    iout[4] = lit_("BRIT", (ftnlen)4);
	}
	if (ischar_(&iin[38]) == 1) {
	    movcw_(&iin[38], cfile, (ftnlen)8);
	    if (brkset_(cfile, brkpntvisrkb2_1.caltyp, (ftnlen)8, (ftnlen)4) 
		    != 0) {
		ret_val = -3;
		return ret_val;
	    }
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "BRKP", (ftnlen)4, (ftnlen)4) == 0) {
	movcw_(&iin[1], cfile, (ftnlen)8);
	if (brkset_(cfile, brkpntvisrkb2_1.caltyp, (ftnlen)8, (ftnlen)4) != 0)
		 {
	    ret_val = -3;
	    return ret_val;
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	clit_(ch__1, (ftnlen)4, &iin[3]);
	if (! isir_(&iin[2], ch__1, &iin[1], (ftnlen)4)) {
	    iout[1] = 2;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("BRIT", (ftnlen)4);
	    iout[4] = lit_("    ", (ftnlen)4);
	    iout[5] = lit_("    ", (ftnlen)4);
	    iout[6] = 1;
	    iout[7] = 1;
	} else {
	    iout[1] = 3;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("TEMP", (ftnlen)4);
	    iout[4] = lit_("BRIT", (ftnlen)4);
	    iout[5] = lit_("    ", (ftnlen)4);
	    iout[6] = lit_("K   ", (ftnlen)4);
	    iout[7] = lit_("    ", (ftnlen)4);
	    iout[8] = 1;
	    iout[9] = 10;
	    iout[10] = 1;
            iout[12] = 10;   /* added TLO */
	}
	ret_val = 0;
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* kb2optvisr_ */

/* Subroutine */ int temptbvisrkb2_(iday, item)
integer *iday, *item;
{
    /* Initialized data */

    static integer irday[2] = { 1974200,1974304 };
    static integer icnt[9] = { 41,60,90,120,150,180,210,230,235 };
    static integer itgat[9] = { 3297,3216,3079,2926,2751,2539,2223,1944,1819 }
	    ;
    static integer irtem1[256] = { 3300,3295,3290,3285,3280,3275,3270,3265,
	    3260,3255,3250,3245,3240,3235,3230,3225,3220,3215,3210,3205,3200,
	    3195,3190,3185,3180,3175,3170,3165,3160,3155,3150,3145,3140,3135,
	    3130,3125,3120,3115,3110,3105,3100,3095,3090,3085,3080,3075,3070,
	    3065,3060,3055,3050,3045,3040,3035,3030,3025,3020,3015,3010,3005,
	    3000,2995,2990,2985,2980,2975,2970,2965,2960,2955,2950,2945,2940,
	    2935,2930,2925,2920,2915,2910,2905,2900,2895,2890,2885,2880,2875,
	    2870,2865,2860,2855,2850,2845,2840,2835,2830,2825,2820,2815,2810,
	    2805,2800,2795,2790,2785,2780,2775,2770,2765,2760,2755,2750,2745,
	    2740,2735,2730,2725,2720,2715,2710,2705,2700,2695,2690,2685,2680,
	    2675,2670,2665,2660,2655,2650,2645,2640,2635,2630,2625,2620,2615,
	    2610,2605,2600,2595,2590,2585,2580,2575,2570,2565,2560,2555,2550,
	    2545,2540,2535,2530,2525,2520,2515,2510,2505,2500,2495,2490,2485,
	    2480,2475,2470,2465,2460,2455,2450,2445,2440,2435,2430,2425,2420,
	    2410,2400,2390,2380,2370,2360,2350,2340,2330,2320,2310,2300,2290,
	    2280,2270,2260,2250,2240,2230,2220,2210,2200,2190,2180,2170,2160,
	    2150,2140,2130,2120,2110,2100,2090,2080,2070,2060,2050,2040,2030,
	    2020,2010,2000,1990,1980,1970,1960,1950,1940,1930,1920,1910,1900,
	    1890,1880,1870,1860,1850,1840,1830,1820,1810,1800,1790,1780,1770,
	    1760,1750,1740,1730,1720,1710,1700,1690,1680,1670,1660,1650,1640,
	    1630 };
    static integer irtot = 2;

    /* System generated locals */
    address a__1[2];
    integer i__1[2], i__2;
    char ch__1[20], ch__2[12];

    /* Builtin functions */
    /* Subroutine */ int s_cat();

    /* Local variables */
    static integer irad;
    static real a, d__;
    static integer i__, j;
    static real t;
    static integer jtemp;
    static real dt;
    static integer ir;
    extern /* Character */ VOID cfi_();
    /* extern / Subroutine / int mctrace_(); */

/* external functions */
/* local variables */
/* initialized variables */
    /* Parameter adjustments */
    --item;

    /* Function Body */
    /* mctrace_(&c__1, "AGETSERV", " in temptb", (ftnlen)8, (ftnlen)10); */
/* Writing concatenation */
    i__1[0] = 8, a__1[0] = " date is";
    cfi_(ch__2, (ftnlen)12, iday);
    i__1[1] = 12, a__1[1] = ch__2;
    s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)20);
    /* mctrace_(&c__1, "AGETSERV", ch__1, (ftnlen)8, (ftnlen)20); */
    for (irad = 0; irad <= 255; ++irad) {
	i__2 = irtot;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    j = i__;
	    if (*iday < irday[i__ - 1]) {
		goto L2;
	    }
/* L1: */
	}
	j = 3;
L2:
	switch ((int)j) {
	    case 1:  goto L10;
	    case 2:  goto L20;
	    case 3:  goto L30;
	}
L10:
	for (i__ = 2; i__ <= 9; ++i__) {
	    j = i__ - 1;
/* L11: */
	    if (irad <= icnt[i__ - 1]) {
		goto L12;
	    }
	}
	j = 8;
L12:
	jtemp = (itgat[j] - itgat[j - 1]) * (irad - icnt[j - 1]) / (icnt[j] - 
		icnt[j - 1]) + itgat[j - 1];
	item[irad + 1] = jtemp;
	goto L300;
L20:
	ir = irad + 1;
	t = (real) irtem1[ir - 1] / (float)10.;
	d__ = (real) (*iday - 1974000);
	a = d__ * (d__ * (d__ * (float)1.443e-6 - (float).001195) + (float)
		.3385) - (float)31.97;
	dt = a * (t * ((float)-.031 - t * (float)2e-5) + (float)3.8);
	item[ir] = (integer) ((t + dt) * (float)10.);
	goto L300;
L30:
	ir = irad + 1;
	item[ir] = irtem1[ir - 1];
L300:
	;
    }
    return 0;
} /* temptbvisrkb2_ */

