/* kb2msat.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer jtype, isou, ides, jopt[5];
} metxxxmsatkb2_;

#define metxxxmsatkb2_1 metxxxmsatkb2_

struct {
    char caltyp[4];
} brkpntmsatkb2_;

#define brkpntmsatkb2_1 brkpntmsatkb2_

/* Table of constant values */

static integer c__5 = 5;
static integer c__0 = 0;
static integer c__34 = 34;

integer kb2inimsat_(char *cin, char *cout, integer *iopt, ftnlen cin_len, 
	ftnlen cout_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int movw_(integer *, integer *, integer *);

/* symbolic constants & shared data */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.1 2000/07/12 13:12:23 gad Exp $ *** */
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


/* ----- MAXAREARQSTLEN - max length of area request string */

/* external functions */
/* local variables */
    /* Parameter adjustments */
    --iopt;

    /* Function Body */
    movw_(&c__5, &iopt[1], metxxxmsatkb2_1.jopt);
    metxxxmsatkb2_1.jtype = 0;
    metxxxmsatkb2_1.isou = iopt[1];
    metxxxmsatkb2_1.ides = iopt[2];
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "RAD ", (
	    ftnlen)4, (ftnlen)4) == 0) {
	metxxxmsatkb2_1.jtype = 1;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "TEMP", (
	    ftnlen)4, (ftnlen)4) == 0) {
	metxxxmsatkb2_1.jtype = 2;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "BRIT", (
	    ftnlen)4, (ftnlen)4) == 0) {
	metxxxmsatkb2_1.jtype = 3;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "MODB", (
	    ftnlen)4, (ftnlen)4) == 0) {
	metxxxmsatkb2_1.jtype = 3;
    }
    if (metxxxmsatkb2_1.jtype == 0) {
	goto L900;
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* kb2inimsat_ */

integer kb2calmsat_(integer *calb, integer *idir, integer *nval, integer *
	jband, integer *ibuf)
{
    /* Initialized data */

    static real ycal[2] = { .04f,.008f };
    static integer m4 = 54516;
    static integer m5 = 54517;
    static integer m6 = 54518;
    static integer m7 = 54519;
    static integer irad[612]	/* was [34][18] */ = { 217,271,335,409,494,
	    591,700,823,959,1111,1277,1459,1658,1873,2106,2356,2623,2910,3214,
	    3537,3879,4239,4619,5017,5435,5872,6327,6802,7296,7808,8339,8889,
	    9457,10043,12,17,25,35,49,66,89,117,153,196,250,315,392,484,592,
	    719,866,1035,1229,1450,1701,1983,2299,2651,3043,3477,3954,4479,
	    5052,5677,6357,7093,7889,8745,230,287,354,432,521,622,737,865,
	    1008,1167,1341,1531,1739,1963,2206,2467,2746,3044,3362,3698,4054,
	    4429,4825,5239,5674,6128,6602,7096,7609,8141,8693,9264,9854,10463,
	    14,21,30,43,59,80,107,141,183,235,298,375,467,575,703,852,1024,
	    1223,1451,1709,2002,2331,2700,3111,3567,4071,4626,5235,5900,6625,
	    7411,8263,9183,10172,300,475,587,717,867,1037,1230,1446,1687,1954,
	    2248,2570,2921,3302,3713,4155,4630,5136,5676,6248,6854,7494,8167,
	    8875,9616,10392,11201,12045,12922,13833,14777,15755,16765,17809,7,
	    15,22,31,43,60,81,107,141,183,235,299,375,466,574,702,850,1023,
	    1221,1449,1707,2000,2330,2700,3113,3571,4078,4636,5250,5921,6653,
	    7448,8310,9241,368,478,590,721,871,1042,1236,1453,1695,1963,2258,
	    2582,2934,3317,3730,4175,4651,5160,5702,6277,6886,7529,8206,8917,
	    9662,10441,11255,12103,12985,13900,14850,15832,16848,17897,10,18,
	    26,37,52,72,98,130,172,223,286,363,457,568,700,856,1038,1248,1492,
	    1770,2087,2447,2851,3305,3812,4375,4999,5686,6441,7268,8169,9150,
	    10212,11362,348,452,559,684,827,990,1175,1382,1614,1870,2153,2462,
	    2800,3166,3563,3989,4446,4935,5455,6008,6593,7211,7861,8545,9262,
	    10012,10795,11612,12461,13343,14257,15204,16183,17194,10,17,25,35,
	    49,68,92,122,161,209,269,341,429,534,658,805,977,1176,1405,1668,
	    1967,2307,2689,3118,3598,4130,4720,5370,6085,6867,7721,8649,9656,
	    10744,387,505,624,761,919,1099,1302,1530,1784,2065,2374,2713,3082,
	    3482,3913,4378,4876,5407,5972,6572,7207,7877,8582,9323,10098,
	    10910,11756,12638,13555,14507,15493,16514,17569,18658,8,15,22,32,
	    44,61,82,110,145,188,242,307,385,479,591,722,876,1054,1259,1494,
	    1762,2065,2406,2789,3217,3692,4217,4797,5433,6130,6890,7716,8611,
	    9580,387,503,621,758,915,1094,1296,1523,1776,2055,2363,2700,3067,
	    3466,3896,4358,4853,5382,5945,6543,7175,7842,8544,9281,10053,
	    10861,11704,12582,13495,14443,15425,16442,17492,18577,8,15,22,31,
	    44,61,82,110,144,187,241,306,384,478,589,720,873,1051,1256,1490,
	    1758,2060,2401,2784,3211,3685,4210,4790,5426,6122,6881,7707,8602,
	    9570,537,667,822,1002,1207,1441,1704,1999,2328,2690,3089,3526,
	    4000,4514,5069,5665,6303,6983,7707,8474,9285,10141,11040,11984,
	    12973,14006,15083,16205,17371,18580,19833,21130,22469,23850,13,21,
	    30,43,60,82,111,147,193,250,320,405,507,628,772,941,1138,1366,
	    1627,1927,2267,2651,3083,3566,4105,4703,5363,6089,6886,7756,8705,
	    9734,10849,12052,534,664,818,996,1200,1433,1695,1988,2315,2676,
	    3072,3506,3978,4490,5041,5634,6268,6945,7664,8427,9234,10084,
	    10979,11918,12901,13928,14999,16115,17274,18477,19723,21012,22343,
	    23717,11,20,30,42,59,80,108,144,189,244,313,396,496,615,756,922,
	    1115,1339,1596,1890,2224,2602,3026,3502,4031,4619,5268,5983,6767,
	    7624,8558,9571,10669,11854 };
    static integer itemp = 165;
    static integer inc = 5;
    static integer lasara = -999;
    static integer lastyp = -1;
    static integer ip2 = 88224;
    static integer newcal = 89128;
    static integer lasbnd = -999;
    static integer m3 = 54515;

    /* System generated locals */
    integer ret_val, i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_nint(real *);

    /* Local variables */
    static real a, b, e;
    static integer i__, j, ie;
    static real bbc, xco;
    static integer ical;
    static real coef[136]	/* was [4][34] */;
    static integer ioff;
    extern real fval_(integer *, real *, real *, real *);
    static real xcal[2], xrad[34];
    static integer isen, iband;
    extern /* Subroutine */ int edest_(char *, integer *, ftnlen);
    extern integer grysclmsatkb2_(real *);
    static real xtemp[34];
    extern /* Subroutine */ int asspl2_(integer *, real *, real *, real *);
    static integer itable[256];
    extern integer brkval_(real *);
    extern /* Subroutine */ int mpixtb_(integer *, integer *, integer *, 
	    integer *, integer *);

/* symbolic constants & shared data */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.1 2000/07/12 13:12:23 gad Exp $ *** */
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


/* ----- MAXAREARQSTLEN - max length of area request string */

/* external functions */
/* local variables */
/* initialized variables */
/* DATA M3/ #0000D4F3 /,M4/ #0000D4F4 / */
/* NOTE #D4F3= 54515 , #D4F4=54516 */
    /* Parameter adjustments */
    --ibuf;
    --idir;
    --calb;

    /* Function Body */
    ret_val = 0;
    iband = *jband;
    if (iband == 0) {
	if (idir[19] == 1) {
	    iband = 1;
	}
	if (idir[19] == 128) {
	    iband = 8;
	}
	if (idir[19] == 512) {
	    iband = 10;
	}
    }
    if (lasara != idir[33] || metxxxmsatkb2_1.jtype != lastyp || lasbnd != 
	    iband) {
	lasbnd = iband;
	lasara = idir[33];
	lastyp = metxxxmsatkb2_1.jtype;
	if (iband != 8 && iband != 10) {
	    if (s_cmp(brkpntmsatkb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) ==
		     0 || s_cmp(brkpntmsatkb2_1.caltyp, "RAW", (ftnlen)4, (
		    ftnlen)3) == 0) {
		for (j = 0; j <= 255; ++j) {
		    r__1 = (real) j;
		    itable[j] = brkval_(&r__1);
/* L1: */
		}
	    } else {
		for (j = 0; j <= 255; ++j) {
		    itable[j] = j;
/* L5: */
		}
	    }
	    goto L150;
	}
	xco = 5.f;
/*  (we won't allow negative radiances */
/*  Default zero radiance level = 5 DN */
	b = 0.f;
/*  Default slope (should never be use */
	ioff = 1;
/*    Tables alternate 11 MU & 6 MU */
/*  Default Offset into Temp tables */
	if (idir[4] >= ip2) {
	    ioff = 2;
	}
	if (idir[4] >= newcal) {
	    b = idir[22] / 1e5f;
/* Slope constant for radiance */
	    xco = idir[23] / 10.f;
/* Offset constant (zero radiance */
	    isen = idir[24];
/* Primary or backup sensor ID (M5 */
	    ical = 1;
/* Index to cal constants for band */
	    if (iband == 10) {
		ical = 2;
	    }
/* Index to cal constants for band */
	    if (isen < 0 || isen > 2) {
		edest_("Invalid sensor ID for band ", &iband, (ftnlen)27);
		edest_("Valid IR sensors are 1, 2, or 0, not ", &isen, (
			ftnlen)37);
		ret_val = -1;
	    }
	    if (idir[21] == m3 && iband == 8) {
		ioff = 3;
	    } else if (idir[21] == m3 && iband == 10) {
		ioff = 4;
	    } else if (idir[21] == m4 && iband == 8) {
		ioff = 5;
	    } else if (idir[21] == m4 && iband == 10) {
		ioff = 6;
	    } else if (idir[21] == m5 && iband == 8 && isen == 1) {
		ioff = 7;
/*  I */
	    } else if (idir[21] == m5 && iband == 10 && isen == 1) {
		ioff = 8;
/*  W */
	    } else if (idir[21] == m5 && iband == 8 && isen == 2) {
		ioff = 9;
/*  I */
	    } else if (idir[21] == m5 && iband == 10 && isen == 2) {
		ioff = 10;
/*  W */
	    } else if (idir[21] == m6 && iband == 8 && isen == 1) {
		ioff = 11;
/*  I */
	    } else if (idir[21] == m6 && iband == 10 && isen == 1) {
		ioff = 12;
/*  W */
	    } else if (idir[21] == m6 && iband == 8 && isen == 2) {
		ioff = 13;
/*  I */
	    } else if (idir[21] == m6 && iband == 10 && isen == 2) {
		ioff = 14;
/*  W */
	    } else if (idir[21] == m7 && iband == 8 && isen == 1) {
		ioff = 15;
/*  I */
	    } else if (idir[21] == m7 && iband == 10 && isen == 1) {
		ioff = 16;
/*  W */
	    } else if (idir[21] == m7 && iband == 8 && isen == 2) {
		ioff = 17;
/*  I */
	    } else if (idir[21] == m7 && iband == 10 && isen == 2) {
		ioff = 18;
/*  W */
	    } else {
		edest_("TABLE UNIDENTIFIED: IDIR(21)=", &idir[21], (ftnlen)29)
			;
		edest_("                       IBAND=", &iband, (ftnlen)29);
		edest_("                      SENSOR=", &idir[24], (ftnlen)29)
			;
		edest_("                      OFFSET=", &ioff, (ftnlen)29);
	    }
	} else if (idir[21] == 0) {
	    a = ycal[ical - 1];
	    b = 1.f;
	    b *= a;
	} else {
	    bbc = idir[21] / 100.f;
	    xcal[0] = idir[22] / 1e6f;
	    xcal[1] = idir[23] / 1e6f;
	    if (idir[23] < 5000) {
		xcal[1] = idir[23] / 1e5f;
	    }
	    b = 121.f / bbc;
	    a = xcal[ical - 1];
	    b *= a;
	}
	if (b == 0.f) {
	    edest_("ERROR: Radiance slope = 0.0", &c__0, (ftnlen)27);
	    edest_("       Check IDIR(21-24)", &c__0, (ftnlen)24);
	}
	if (s_cmp(brkpntmsatkb2_1.caltyp, "RAW", (ftnlen)4, (ftnlen)3) == 0 &&
		 metxxxmsatkb2_1.jtype == 3) {
	    for (i__ = 0; i__ <= 255; ++i__) {
		r__1 = (real) i__;
		itable[i__] = brkval_(&r__1);
/* L10: */
	    }
	    goto L150;
	}
	if (s_cmp(brkpntmsatkb2_1.caltyp, "RAD", (ftnlen)4, (ftnlen)3) == 0 &&
		 metxxxmsatkb2_1.jtype == 3) {
	    for (i__ = 0; i__ <= 255; ++i__) {
/* Computing MAX */
		r__1 = 0.f, r__2 = b * (i__ - xco);
		e = max(r__1,r__2);
		r__1 = e;
		itable[i__] = brkval_(&r__1);
/* L15: */
	    }
	    goto L150;
	} else {
	    for (i__ = 0; i__ <= 255; ++i__) {
		e = b * (i__ - xco);
/* Computing MAX */
		r__1 = e * 1e3f;
		i__1 = 0, i__2 = i_nint(&r__1);
		ie = max(i__1,i__2);
		itable[i__] = ie;
/* L20: */
	    }
	    if (metxxxmsatkb2_1.jtype == 1) {
		goto L150;
	    }
	}
	for (i__ = 1; i__ <= 34; ++i__) {
	    xtemp[i__ - 1] = (itemp + (i__ - 1) * inc) * 10.f;
	    xrad[i__ - 1] = (real) irad[i__ + ioff * 34 - 35];
/* L30: */
	}
	asspl2_(&c__34, xrad, xtemp, coef);
	for (i__ = 1; i__ <= 256; ++i__) {
	    if (s_cmp(brkpntmsatkb2_1.caltyp, "TEMP", (ftnlen)4, (ftnlen)4) ==
		     0 && metxxxmsatkb2_1.jtype == 3) {
		r__2 = (real) itable[i__ - 1];
		r__1 = fval_(&c__34, &r__2, xrad, coef) / 10;
		itable[i__ - 1] = brkval_(&r__1);
	    } else {
		r__2 = (real) itable[i__ - 1];
		r__1 = fval_(&c__34, &r__2, xrad, coef);
		itable[i__ - 1] = i_nint(&r__1);
	    }
/* L60: */
	}
	if (metxxxmsatkb2_1.jtype == 2) {
	    goto L150;
	}
	if (s_cmp(brkpntmsatkb2_1.caltyp, "TEMP", (ftnlen)4, (ftnlen)4) == 0 
		&& metxxxmsatkb2_1.jtype == 3) {
	    goto L150;
	}
	if (s_cmp(brkpntmsatkb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 0 
		&& metxxxmsatkb2_1.jtype == 3) {
	    for (i__ = 1; i__ <= 256; ++i__) {
		r__2 = itable[i__ - 1] / 10.f;
		r__1 = (real) grysclmsatkb2_(&r__2);
		itable[i__ - 1] = brkval_(&r__1);
/* L70: */
	    }
	} else {
	    for (i__ = 1; i__ <= 256; ++i__) {
		r__1 = itable[i__ - 1] / 10.f;
		itable[i__ - 1] = grysclmsatkb2_(&r__1);
/* L80: */
	    }
	}
L150:
	;
    }
    mpixtb_(nval, &metxxxmsatkb2_1.isou, &metxxxmsatkb2_1.ides, &ibuf[1], 
	    itable);
    return ret_val;
} /* kb2calmsat_ */

integer kb2optmsat_(char *cfunc, integer *iin, integer *iout, ftnlen 
	cfunc_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer lit_(char *, ftnlen);
    static char cfile[8];
    extern /* Subroutine */ int movwc_(integer *, char *, ftnlen);
    extern integer ischar_(integer *), brkset_(char *, char *, ftnlen, ftnlen)
	    ;

/* symbolic constants & shared data */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.1 2000/07/12 13:12:23 gad Exp $ *** */
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


/* ----- MAXAREARQSTLEN - max length of area request string */

/* external functions */
/* local variables */
    /* Parameter adjustments */
    --iout;
    --iin;

    /* Function Body */
    if (s_cmp(cfunc, "KEYS", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[4] == 8 || iin[4] == 10) {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	} else {
	    iout[1] = 2;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("BRIT", (ftnlen)4);
	}
	if (ischar_(&iin[38]) == 1) {
	    movwc_(&iin[38], cfile, (ftnlen)8);
	    if (brkset_(cfile, brkpntmsatkb2_1.caltyp, (ftnlen)8, (ftnlen)4) 
		    != 0) {
		ret_val = -3;
		return ret_val;
	    }
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "BRKP", (ftnlen)4, (ftnlen)4) == 0) {
	movwc_(&iin[1], cfile, (ftnlen)8);
	if (brkset_(cfile, brkpntmsatkb2_1.caltyp, (ftnlen)8, (ftnlen)4) != 0)
		 {
	    ret_val = -3;
	    return ret_val;
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[1] < 1 || iin[1] > 12) {
	    ret_val = -2;
	    return ret_val;
	}
	if (iin[1] == 8 || iin[1] == 10) {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	    iout[6] = lit_("    ", (ftnlen)4);
	    iout[7] = lit_("WP**", (ftnlen)4);
	    iout[8] = lit_("K   ", (ftnlen)4);
	    iout[9] = lit_("    ", (ftnlen)4);
	    iout[10] = 1;
	    iout[11] = 1000;
	    iout[12] = 10;
	    iout[13] = 1;
	    ret_val = 0;
	} else {
	    iout[1] = 2;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("BRIT", (ftnlen)4);
	    iout[4] = lit_("    ", (ftnlen)4);
	    iout[5] = lit_("    ", (ftnlen)4);
	    iout[6] = 1;
	    iout[7] = 1;
	    ret_val = 0;
	}
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* kb2optmsat_ */

integer grysclmsatkb2_(real *tempk)
{
    /* Initialized data */

    static integer con1 = 418;
    static integer con2 = 660;
    static real tlim = 242.f;

    /* System generated locals */
    integer ret_val, i__1;

/* initialized variables */
    if (*tempk >= tlim) {
	goto L100;
    }
/* Computing MIN */
    i__1 = con1 - (integer) (*tempk);
    ret_val = min(i__1,255);
    goto L200;
L100:
/* Computing MAX */
    i__1 = con2 - (integer) (*tempk * 2);
    ret_val = max(i__1,0);
L200:
    return ret_val;
} /* grysclmsatkb2_ */

