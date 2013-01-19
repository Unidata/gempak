/* kb2gms.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer jtype, isou, ides, kopt, jopt[5];
} gmsxxgmskb2_;

#define gmsxxgmskb2_1 gmsxxgmskb2_

union {
    struct {
	integer ival;
	real xtab[1024];
	integer itab[1024], jtab[1024];
    } _1;
    struct {
	integer ival;
	real xtab[1024];
	integer itabr[1024], jtab[1024];
    } _2;
} debuggmskb2_;

#define debuggmskb2_1 (debuggmskb2_._1)
#define debuggmskb2_2 (debuggmskb2_._2)

struct {
    char caltyp[4];
} brkpntgmskb2_;

#define brkpntgmskb2_1 brkpntgmskb2_

struct {
    integer ktab[256];
} radgmskb2_;

#define radgmskb2_1 radgmskb2_

union {
    struct {
	real cwn[3], fk1[3], fk2[3], tc[6]	/* was [2][3] */;
    } _1;
    struct {
	real cwn[3], fk1[3], fk2[3], tcc[6]	/* was [2][3] */;
    } _2;
} gmspfcgmskb2_;

#define gmspfcgmskb2_1 (gmspfcgmskb2_._1)
#define gmspfcgmskb2_2 (gmspfcgmskb2_._2)

/* Table of constant values */

static integer c__5 = 5;
static integer c__0 = 0;
static integer c__3 = 3;
static integer c__151 = 151;
static integer c__7168 = 7168;
static integer c__256 = 256;
static integer c__4 = 4;
static integer c__6 = 6;
static integer c__1 = 1;
static integer c__1024 = 1024;
static real c_b208 = 1.f;
static integer c__512 = 512;
static integer c__2 = 2;
static integer c__152 = 152;

integer kb2inigms_(char *cin, char *cout, integer *iopt, ftnlen cin_len, 
	ftnlen cout_len)
{
    /* System generated locals */
    address a__1[3];
    integer ret_val, i__1[3];
    char ch__1[31];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int movw_(integer *, integer *, integer *), 
	    edest_(char *, integer *, ftnlen);
    extern integer lwfile_(char *, ftnlen);
    static integer ierror;

/*  All variables must be declared */
/*  Input pixel type (RAW, BRIT) */
/*  Output pixel type (TEMP, ALB, RAD, RAW, */
/*  Input calibration parameters */
/*     (defines NUMAREAOPTIONS) */
/*  Global declarations for McIDAS a */
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

/*  Flag identifying conversion code */
/*  Source pixel size in bytes */
/*  Destination pixel size in bytes */
/*  Flag specifying how to construct */
/*  Calibration parameters for conve */
/*  Tests for LW file presence */
/*  type, and calibration file test */
/*  Error flag for byte size, conver */
    /* Parameter adjustments */
    --iopt;

    /* Function Body */
    movw_(&c__5, &iopt[1], gmsxxgmskb2_1.jopt);
    ierror = 0;
    gmsxxgmskb2_1.jtype = 0;
    gmsxxgmskb2_1.isou = iopt[1];
    gmsxxgmskb2_1.ides = iopt[2];
    if (gmsxxgmskb2_1.isou != 1 || gmsxxgmskb2_1.ides != 1 && 
	    gmsxxgmskb2_1.ides != 2 && gmsxxgmskb2_1.ides != 4) {
	ierror = 1;
	edest_("KBX_INI: Invalid byte sizes specified.", &c__0, (ftnlen)38);
	edest_("Source (must be 1)=", &gmsxxgmskb2_1.isou, (ftnlen)19);
	edest_("Destination (must be 1,2, or 4)=", &gmsxxgmskb2_1.ides, (
		ftnlen)32);
    }
    if ((s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(cin, "RAW", (
	    ftnlen)4, (ftnlen)3) == 0) && s_cmp(cout, "TEMP", (ftnlen)4, (
	    ftnlen)4) == 0) {
	gmsxxgmskb2_1.jtype = 1;
    }
    if ((s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(cin, "RAW", (
	    ftnlen)4, (ftnlen)3) == 0) && s_cmp(cout, "ALB", (ftnlen)4, (
	    ftnlen)3) == 0) {
	gmsxxgmskb2_1.jtype = 2;
    }
    if ((s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(cin, "RAW", (
	    ftnlen)4, (ftnlen)3) == 0) && s_cmp(cout, "RAD", (ftnlen)4, (
	    ftnlen)3) == 0) {
	gmsxxgmskb2_1.jtype = 3;
    }
    if (s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "MODB", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gmsxxgmskb2_1.jtype = 4;
    }
    if ((s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(cin, "RAW", (
	    ftnlen)4, (ftnlen)3) == 0) && s_cmp(cout, "RAW", (ftnlen)4, (
	    ftnlen)3) == 0) {
	gmsxxgmskb2_1.jtype = 5;
    }
    if ((s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(cin, "RAW", (
	    ftnlen)4, (ftnlen)3) == 0) && s_cmp(cout, "BRIT", (ftnlen)4, (
	    ftnlen)4) == 0) {
	gmsxxgmskb2_1.jtype = 6;
    }
    if (s_cmp(cin, "RAW", (ftnlen)4, (ftnlen)3) == 0 && s_cmp(cout, "MODB", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gmsxxgmskb2_1.jtype = 7;
    }
    if (gmsxxgmskb2_1.jtype == 0) {
	edest_("KBX_CAL data conversion unrecognized:", &c__0, (ftnlen)37);
/* Writing concatenation */
	i__1[0] = 24, a__1[0] = "          Input  type-->";
	i__1[1] = 4, a__1[1] = cin;
	i__1[2] = 3, a__1[2] = "<--";
	s_cat(ch__1, a__1, i__1, &c__3, (ftnlen)31);
	edest_(ch__1, &c__0, (ftnlen)31);
/* Writing concatenation */
	i__1[0] = 24, a__1[0] = "          Output type-->";
	i__1[1] = 4, a__1[1] = cout;
	i__1[2] = 3, a__1[2] = "<--";
	s_cat(ch__1, a__1, i__1, &c__3, (ftnlen)31);
	edest_(ch__1, &c__0, (ftnlen)31);
	ierror = 1;
    }
    if (lwfile_("GMSCAL", (ftnlen)6) == 0) {
	edest_("KBX_INI error, LW file GMSCAL not found", &c__0, (ftnlen)39);
	ierror = 1;
    }
    if (lwfile_("GMSCALU", (ftnlen)7) == 0) {
    }
    if (ierror != 0) {
	goto L900;
    }
    gmsxxgmskb2_1.kopt = 0;
/*  Initialize calibration table generation to try all m */
    ret_val = 0;
    goto L999;
L900:
    ret_val = -1;
L999:
    return ret_val;
} /* kb2inigms_ */

integer kb2calgms_(integer *calb, integer *idir, integer *nval, integer *
	iband, integer *ibuf)
{
    /* Initialized data */

    static integer ivflg = 0;
    static integer lastyp = -1;
    static integer laschan = -1;
    static integer lasband = -1;

    /* System generated locals */
    integer ret_val;
    real r__1, r__2;
    static integer equiv_1[1024];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_nint(real *);

    /* Local variables */
    static integer i__, k;
    extern integer rdcalgmskb2_(integer *, integer *, integer *, integer *);
#define itab (equiv_1)
    static real xalb;
    extern real vplancgmskb2_(real *, integer *, integer *, integer *);
    static real xrad;
    static integer ichan, irtab;
    extern /* Subroutine */ int edest_(char *, integer *, ftnlen);
#define itabv (equiv_1)
#define jtabv ((integer *)&debuggmskb2_2 + 2049)
    static integer ibrit, ivisn;
    static real xtemp;
    extern integer brkval_(real *);
    extern /* Subroutine */ int gryscl_(real *, integer *), mpixtb_(integer *,
	     integer *, integer *, integer *, integer *);

/*  All variables must be declared */
/*     from each line header for IBUF dat */
/*  Input array of calibration constants */
/*     for GMS, since the area calibratio */
/*     block must be accessed locally) */
/*  Area directory buffer (this is mandat */
/*  Number of pixels to process from inpu */
/*  Band number (not needed for GMS-4) */
/*     (will contain converted values at */
/*  I/O array containing pixels to be mod */
/*     (defines NUMAREAOPTIONS) */
/*  Global declarations for McIDAS a */
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

/*  Flag identifying conversion code */
/*  Source pixel size in bytes */
/*  Destination pixel size in bytes */
/*  Flag specifying how to construct */
/*  Calibration parameters for conve */
/*  Common block for BRKSET table type */
/*  Calibration type for breakpoint */
/*    to generate the ITAB/ITABR arr */
/*  Identifies method used by RD_CAL */
/*    produced by RD_CAL function */
/*    and returned as ITAB.  It is */
/*    renamed ITABR here to avoid do */
/*    defining the array. */
/*  stage 1 conversion table */
/*    to the input DN value: either */
/*    albedo, temperature, or radian */
/*    (this quantity is converted to */
/*    scaled integer appearing in JT */
/*  Real physical quantity correspon */
/*    produced by KBX_CAL here */
/*    (generates conversion transfer */
/*     function table from albedo or */
/*     temperature to scaled integer */
/*     output, using BRKVAL and MPIX */
/*  stage 2 conversion table */
/*     visible sensors are identifie */
/*     by IVISN=1,2,3,4.  For IR ban */
/*     the value of IVISN=1 always, */
/*     only 1/4 of the table is used */
/*  JTAB array for visible channel w */
/*  Provides values from breakpoint */
/*  Planck function */
/*  Sets up the ITAB lookup table */
/*  index variable */
/*  index variable */
/*    (high BRIT on screen means low */
/*  Output of GRYSCAL TEMP-->BRIT co */
/*     *** may have to increase to */
/*         for GMS-5 *** */
/*  visible or IR channel designator */
/*     (0 means unknown or undefined */
/*  Visible sensor designator 0-4 */
/*    (returned ALB output is */
/*     in %, i.e. albedo*100.) */
/*  true albedo 0.000-1.000 */
/*  true absolute temperature (Kelvi */
/*  Radiance (mW/m**2/ster/cm**-1) */
/*  Identifies current channel as IR */
/*    produced by RD_CAL function */
/*    (generates a calibrated transf */
/*     function table from the physi */
/*     conversion formulas -- input */
/*     index is a function of DN val */
/*     and IVISN, output is either a */
/*     albedo or is in degrees Kelvi */
/*  stage 1 conversion table */
/*     visible sensors are identifie */
/*     by IVISN=1,2,3,4 */
/*  ITAB array for visible channel w */
/*  if set to 1 means visible data */
/*  JTYPE for which tables are curre */
/*  ICHAN for which tables are curre */
/*  IBAND for which tables are curre */
    /* Parameter adjustments */
    --ibuf;
    --idir;
    --calb;

    /* Function Body */
    if (idir[3] == 12) {
	ichan = 0;
	irtab = 0;
    } else if (idir[3] == 13) {
	ichan = 1;
	irtab = 1;
    } else if (idir[3] == 82 && *iband == 1) {
	ichan = 0;
	irtab = 0;
    } else if (idir[3] == 82 && *iband == 2) {
	ichan = 1;
	irtab = 1;
    } else if (idir[3] == 82 && *iband == 8) {
	ichan = 1;
	irtab = 1;
    } else if (idir[3] == 83 && *iband == 1) {
	ichan = 0;
	irtab = 0;
    } else if (idir[3] == 83 && *iband == 2) {
	ichan = 1;
	irtab = 1;
    } else if (idir[3] == 83 && *iband == 3) {
	ichan = 1;
	irtab = 1;
    } else if (idir[3] == 83 && *iband == 4) {
	ichan = 1;
	irtab = 1;
    } else if (idir[3] == 83 && *iband == 8) {
	ichan = 1;
	irtab = 1;
    } else {
	edest_("KBX_CAL: Unrecognized data band or SS", &c__0, (ftnlen)37);
	ret_val = -1;
    }
    if (*iband == 1) {
	if (calb[2] == 1819017216) {
	    ivisn = 1;
	} else if (calb[2] == -1263271936) {
	    ivisn = 2;
	} else if (calb[2] == -656932864) {
	    ivisn = 3;
	} else if (calb[2] == -50593792) {
	    ivisn = 4;
	} else {
	    ivisn = 1;
	}
    } else {
	ivisn = 1;
    }
    if (s_cmp(brkpntgmskb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 0 && 
	    gmsxxgmskb2_1.jtype == 4 && lastyp != gmsxxgmskb2_1.jtype || 
	    s_cmp(brkpntgmskb2_1.caltyp, "RAW", (ftnlen)4, (ftnlen)3) == 0 && 
	    gmsxxgmskb2_1.jtype == 7 && lastyp != gmsxxgmskb2_1.jtype) {
	for (i__ = 1; i__ <= 256; ++i__) {
	    r__1 = (real) (i__ - 1);
	    debuggmskb2_2.jtab[i__ - 1] = brkval_(&r__1);
	}
	lastyp = gmsxxgmskb2_1.jtype;
	ret_val = 0;
    } else if (gmsxxgmskb2_1.jtype == 5 && lastyp != gmsxxgmskb2_1.jtype) {
	for (i__ = 1; i__ <= 256; ++i__) {
	    debuggmskb2_2.jtab[i__ - 1] = i__ - 1;
	}
	lastyp = gmsxxgmskb2_1.jtype;
	ret_val = 0;
    } else if (lastyp != gmsxxgmskb2_1.jtype || laschan != ichan || lasband !=
	     *iband) {
	if (ichan == 0 && (ivflg == 0 || lastyp != gmsxxgmskb2_1.jtype || 
		lasband != *iband)) {
	    laschan = ichan;
	    lasband = *iband;
	    if (rdcalgmskb2_(&calb[1], &idir[1], iband, itab) != 0) {
		edest_("KBX_CAL: RD_CAL call failed.", &c__0, (ftnlen)28);
		ret_val = -1;
	    }
	    ivflg = 1;
	    if (gmsxxgmskb2_1.jtype == 2 || gmsxxgmskb2_1.jtype == 4 || 
		    gmsxxgmskb2_1.jtype == 7) {
		for (i__ = 1; i__ <= 1024; ++i__) {
		    xalb = itab[i__ - 1] / 1e3f;
		    debuggmskb2_2.xtab[i__ - 1] = xalb;
		    if (s_cmp(brkpntgmskb2_1.caltyp, "ALB", (ftnlen)4, (
			    ftnlen)3) == 0 && gmsxxgmskb2_1.jtype != 2) {
/* MODB */
			r__1 = (xalb + .5f) / 10.f;
			debuggmskb2_2.jtab[i__ - 1] = brkval_(&r__1);
		    } else if (gmsxxgmskb2_1.jtype == 2) {
/* ALB */
			debuggmskb2_2.jtab[i__ - 1] = (integer) (xalb + .5f);
		    }
		}
		lastyp = gmsxxgmskb2_1.jtype;
	    }
	    if (gmsxxgmskb2_1.jtype == 6 || gmsxxgmskb2_1.jtype == 7) {
		if (s_cmp(brkpntgmskb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4)
			 == 0 && gmsxxgmskb2_1.jtype != 6) {
/* MODB */
		    for (i__ = 1; i__ <= 256; ++i__) {
			r__1 = (real) itab[i__ - 1] / 4e3f;
			k = brkval_(&r__1);
			debuggmskb2_2.jtab[i__ - 1] = k;
			debuggmskb2_2.jtab[i__ + 255] = k;
			debuggmskb2_2.jtab[i__ + 511] = k;
			debuggmskb2_2.jtab[i__ + 767] = k;
		    }
		} else if (gmsxxgmskb2_1.jtype == 6) {
/* BRIT */
		    for (i__ = 1; i__ <= 1024; ++i__) {
			debuggmskb2_2.jtab[i__ - 1] = itab[i__ - 1] / 4e3f;
			debuggmskb2_2.jtab[i__ - 1] = debuggmskb2_2.jtab[i__ 
				- 1] * .80000000000000004f + 35.f;
		    }
		}
		ivisn = 1;
		lastyp = gmsxxgmskb2_1.jtype;
	    }
	} else if (ichan == 1 && lastyp != gmsxxgmskb2_1.jtype || lasband != *
		iband) {
	    laschan = ichan;
	    lasband = *iband;
	    if (rdcalgmskb2_(&calb[1], &idir[1], iband, itab) != 0) {
		edest_("KBX_CAL: RD_CAL call failed.", &c__0, (ftnlen)28);
		ret_val = -1;
	    }
	    ivflg = 1;
	    if (gmsxxgmskb2_1.jtype == 1 || gmsxxgmskb2_1.jtype == 4 || 
		    gmsxxgmskb2_1.jtype == 7) {
		for (i__ = 1; i__ <= 256; ++i__) {
		    if (s_cmp(brkpntgmskb2_1.caltyp, "TEMP", (ftnlen)4, (
			    ftnlen)4) == 0 && gmsxxgmskb2_1.jtype != 1) {
/* MODB */
			r__1 = itab[i__ - 1] / 1e3f;
			debuggmskb2_2.jtab[i__ - 1] = brkval_(&r__1);
		    } else if (gmsxxgmskb2_1.jtype == 1) {
/* TEMP */
			debuggmskb2_2.jtab[i__ - 1] = (integer) (itab[i__ - 1]
				 / 100.f + .5f);
		    }
		    xtemp = (real) itab[i__ - 1] / 1e3f;
		    debuggmskb2_2.xtab[i__ - 1] = xtemp;
		}
		ivisn = 1;
	    }
	    if (gmsxxgmskb2_1.jtype == 3 || gmsxxgmskb2_1.jtype == 4 || 
		    gmsxxgmskb2_1.jtype == 7) {
		for (i__ = 1; i__ <= 256; ++i__) {
		    if (s_cmp(brkpntgmskb2_1.caltyp, "RAD ", (ftnlen)4, (
			    ftnlen)4) == 0 && gmsxxgmskb2_1.jtype != 3) {
/* MODB */
			r__2 = itab[i__ - 1] / 1e3f;
			r__1 = vplancgmskb2_(&r__2, &i__, &idir[3], iband) * 
				10;
			debuggmskb2_2.jtab[i__ - 1] = brkval_(&r__1);
		    } else if (gmsxxgmskb2_1.jtype == 3) {
/* RAD */
			r__2 = itab[i__ - 1] / 1e3f;
			r__1 = vplancgmskb2_(&r__2, &i__, &idir[3], iband) * 
				100.f;
			debuggmskb2_2.jtab[i__ - 1] = i_nint(&r__1);
		    }
		    xtemp = (real) itab[i__ - 1] / 1e3f;
		    xrad = vplancgmskb2_(&xtemp, &i__, &idir[3], iband);
		    debuggmskb2_2.xtab[i__ - 1] = xrad;
		}
		ivisn = 1;
	    }
	    if (gmsxxgmskb2_1.jtype == 6 || gmsxxgmskb2_1.jtype == 7) {
		lastyp = gmsxxgmskb2_1.jtype;
		for (i__ = 1; i__ <= 256; ++i__) {
		    xtemp = (real) itab[i__ - 1] / 1e3f;
		    gryscl_(&xtemp, &ibrit);
		    debuggmskb2_2.xtab[i__ - 1] = (real) ibrit;
		    if (s_cmp(brkpntgmskb2_1.caltyp, "BRIT", (ftnlen)4, (
			    ftnlen)4) == 0 && gmsxxgmskb2_1.jtype != 6) {
/* MODB */
			r__1 = (real) ibrit;
			debuggmskb2_2.jtab[i__ - 1] = brkval_(&r__1);
		    } else if (gmsxxgmskb2_1.jtype == 6) {
/* BRIT */
			debuggmskb2_2.jtab[i__ - 1] = ibrit;
		    }
		}
		ivisn = 1;
	    }
	    lastyp = gmsxxgmskb2_1.jtype;
	}
	ret_val = 0;
    } else {
	ret_val = 0;
    }
    mpixtb_(nval, &gmsxxgmskb2_1.isou, &gmsxxgmskb2_1.ides, &ibuf[1], &jtabv[(
	    ivisn << 8) - 256]);
    return ret_val;
} /* kb2calgms_ */

#undef jtabv
#undef itabv
#undef itab


integer kb2optgms_(char *cfunc, integer *iin, integer *iout, ftnlen cfunc_len)
{
    /* System generated locals */
    address a__1[3];
    integer ret_val, i__1[3];
    char ch__1[34];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern integer lit_(char *, ftnlen);
    static char cfile[8];
    extern /* Subroutine */ int edest_(char *, integer *, ftnlen);
    static integer itest;
    extern /* Subroutine */ int movwc_(integer *, char *, ftnlen);
    extern integer ischar_(integer *), brkset_(char *, char *, ftnlen, ftnlen)
	    ;

/*  All variables must be declared */
/*  Option or function descriptor */
/*  Input parameters */
/*  Output parameters */
/*     (defines NUMAREAOPTIONS) */
/*  Global declarations for McIDAS a */
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

/*  Flag identifying conversion code */
/*  Source pixel size in bytes */
/*  Destination pixel size in bytes */
/*  Flag specifying how to construct */
/*  Calibration parameters for conve */
/*  Common block for BRKSET table type */
/*  Calibration type for breakpoint */
/*  Sets breakpoint table values */
/*  Numeric ASCII value of character */
/*  Four byte integer representing C */
/*  Logical AND function */
/*    (stored in frame dir words 38- */
/*  Breakpoint table name for SU */
/*  Validity test variable */
    /* Parameter adjustments */
    --iout;
    --iin;

    /* Function Body */
    if (s_cmp(cfunc, "KEYS", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[1] == 12 || iin[1] >= 82 && iin[4] == 1) {
	    iout[1] = 3;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("ALB ", (ftnlen)4);
	    iout[4] = lit_("BRIT", (ftnlen)4);
	} else if (iin[1] == 13 || iin[1] >= 82 && iin[4] > 1) {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	} else {
	    edest_("KBX_OPT: Cannot identify vis or IR for KEYS option.", &
		    c__0, (ftnlen)51);
	}
	ret_val = 0;
	if (ischar_(&iin[38]) == 1) {
	    movwc_(&iin[38], cfile, (ftnlen)8);
	    if (brkset_(cfile, brkpntgmskb2_1.caltyp, (ftnlen)8, (ftnlen)4) !=
		     0) {
		ret_val = -3;
	    }
	}
    } else if (s_cmp(cfunc, "BRKP", (ftnlen)4, (ftnlen)4) == 0) {
	movwc_(&iin[1], cfile, (ftnlen)8);
	ret_val = 0;
	if (brkset_(cfile, brkpntgmskb2_1.caltyp, (ftnlen)8, (ftnlen)4) != 0) 
		{
	    ret_val = -3;
	}
    } else if (s_cmp(cfunc, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[2] == 12 || iin[2] >= 82 && iin[1] == 1) {
	    iout[1] = 3;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("ALB ", (ftnlen)4);
	    iout[4] = lit_("BRIT", (ftnlen)4);
	    iout[5] = lit_("    ", (ftnlen)4);
	    iout[6] = lit_("  % ", (ftnlen)4);
	    iout[7] = lit_("    ", (ftnlen)4);
	    iout[8] = 1;
	    iout[9] = 10;
	    iout[10] = 1;
	} else if (iin[2] == 13 || iin[2] >= 82 && iin[1] > 1) {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	    iout[6] = lit_("    ", (ftnlen)4);
	    iout[7] = lit_("MW**", (ftnlen)4);
	    iout[8] = lit_("  K ", (ftnlen)4);
	    iout[9] = lit_("    ", (ftnlen)4);
	    iout[10] = 1;
	    iout[11] = 100;
	    iout[12] = 10;
	    iout[13] = 1;
	} else {
	    edest_("KBX_OPT: Cannot identify vis or IR for INFO option.", &
		    c__0, (ftnlen)51);
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "METH", (ftnlen)4, (ftnlen)4) == 0) {
	itest = iin[1] & -16;
	if (itest != 0) {
	    ret_val = -4;
	    gmsxxgmskb2_1.kopt = 0;
	    edest_("KB2OPTgms                                       : Invali"
		    "d calibration table method!  ", &iin[1], (ftnlen)85);
	    edest_("        Must be 1-15", &c__0, (ftnlen)20);
	} else {
	    ret_val = 0;
	    gmsxxgmskb2_1.kopt = iin[1];
	}
    } else {
/* Writing concatenation */
	i__1[0] = 27, a__1[0] = "Unknown KBX_OPT function-->";
	i__1[1] = 4, a__1[1] = cfunc;
	i__1[2] = 3, a__1[2] = "<--";
	s_cat(ch__1, a__1, i__1, &c__3, (ftnlen)34);
	edest_(ch__1, &c__0, (ftnlen)34);
	ret_val = -1;
    }
    return ret_val;
} /* kb2optgms_ */

real vplancgmskb2_(real *t, integer *index, integer *iss, integer *iband)
{
    /* Initialized data */

    static real fk1 = 8102.1f;
    static real fk2 = 1265.4f;
    static real tc1 = .6207f;
    static real tc2 = .99528f;

    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real tt, expn;
    extern /* Subroutine */ int edest_(char *, integer *, ftnlen);

/* Absolute temperature */
/* Table index (DN value) */
/* Spacecraft or instrument ID */
/* Spectral Band for instrument */
/*  (and GMP_FCO/GMT_RAD if not a power s */
/* Radiance table for GMS-5 from RD_CAL */
    if (*iss == 13 || *iss == 82 && *iband == 2 || *iss == 82 && *iband == 8) 
	    {
	tt = tc1 + tc2 * *t;
	expn = exp(fk2 / tt) - 1.f;
	ret_val = fk1 / expn;
    } else if (*iss >= 83 && *iss <= 86) {
	ret_val = radgmskb2_1.ktab[*index - 1] / 1e3f;
    } else {
	edest_("V_PLANC:  Unrecognized SS or BAND", &c__0, (ftnlen)33);
	edest_("              ISS=", iss, (ftnlen)18);
	edest_("            IBAND=", iband, (ftnlen)18);
	ret_val = 0.f;
    }
    return ret_val;
} /* vplancgmskb2_ */

real gmtradgmskb2_(real *t, integer *iband)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static integer k;
    static real tt, expn;

/*  Temperature in degrees Kelvin */
/*  IR band number */
/*  Number of bands */
/*  Number of TC constants */
/*  IR channel array index */
    k = *iband - 1;
    tt = gmspfcgmskb2_1.tc[(k << 1) - 2] + gmspfcgmskb2_1.tc[(k << 1) - 1] * *
	    t;
    expn = exp(gmspfcgmskb2_1.fk2[k - 1] / tt) - 1.f;
    ret_val = gmspfcgmskb2_1.fk1[k - 1] / expn;
    return ret_val;
} /* gmtradgmskb2_ */

/* Subroutine */ int gmpfcogmskb2_(char *cdet, integer *iok, ftnlen cdet_len)
{
    /* Initialized data */

    static char ca[1] = "A";
    static char cb[1] = "B";
    static real awn[3] = { 925.374f,869.613f,1443.508f };
    static real ak1[3] = { 9438.1f,7832.7f,35825.f };
    static real ak2[3] = { 1331.4f,1251.2f,2076.9f };
    static real acc[6]	/* was [2][3] */ = { .52162f,.99819f,.45456f,.99833f,
	    .45842f,.99893f };
    static real bwn[3] = { 929.438f,869.225f,1444.768f };
    static real bk1[3] = { 9563.f,7822.2f,35919.f };
    static real bk2[3] = { 1337.3f,1250.6f,2078.7f };
    static real bcc[6]	/* was [2][3] */ = { .53652f,.99815f,.51422f,.99811f,
	    .4899f,.99886f };

    static integer j, k;

/*  Number of bands */
/*  Number of TC constants */
/*  Index */
/*  Index */
    *iok = 1;
    if (*(unsigned char *)cdet == *(unsigned char *)&ca[0]) {
	for (k = 1; k <= 3; ++k) {
	    gmspfcgmskb2_2.cwn[k - 1] = awn[k - 1];
	    gmspfcgmskb2_2.fk1[k - 1] = ak1[k - 1];
	    gmspfcgmskb2_2.fk2[k - 1] = ak2[k - 1];
	    for (j = 1; j <= 2; ++j) {
		gmspfcgmskb2_2.tcc[j + (k << 1) - 3] = acc[j + (k << 1) - 3];
	    }
	}
    } else if (*(unsigned char *)cdet == *(unsigned char *)&cb[0]) {
	for (k = 1; k <= 3; ++k) {
	    gmspfcgmskb2_2.cwn[k - 1] = bwn[k - 1];
	    gmspfcgmskb2_2.fk1[k - 1] = bk1[k - 1];
	    gmspfcgmskb2_2.fk2[k - 1] = bk2[k - 1];
	    for (j = 1; j <= 2; ++j) {
		gmspfcgmskb2_2.tcc[j + (k << 1) - 3] = bcc[j + (k << 1) - 3];
	    }
	}
    } else {
	*iok = 0;
    }
    return 0;
} /* gmpfcogmskb2_ */

integer rdcalgmskb2_(integer *calb, integer *idir, integer *iband, integer *
	itab)
{
    /* Initialized data */

    static real wnfctr[3] = { .7903f,.7874f,.8676f };

    /* System generated locals */
    address a__1[2];
    integer ret_val, i__1, i__2, i__3[2];
    real r__1;
    char ch__1[55], ch__2[73], ch__3[42];

    /* Builtin functions */
    double r_sign(real *, real *), pow_ri(real *, integer *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double r_mod(real *, real *);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static real c__, g;
    static integer i__, j, k;
    static real r__, v, c0, v0;
    static integer v1, v2, ig[3], iv, ic0[3], iv0[3], ida, dir[512], ihr, imn,
	     imo, iok, jok;
    extern integer lit_(char *, ftnlen), lwi_(char *, integer *, integer *, 
	    integer *, ftnlen);
    static integer iss;
    static real riv;
    static integer iyr, ixv, ifac;
    static real beta[21]	/* was [7][3] */;
    static integer ltab, ioff;
    extern real gmtradgmskb2_(real *, integer *);
    static integer kend;
    extern /* Subroutine */ int gmpfcogmskb2_(char *, integer *, ftnlen);
    static integer nlen;
    extern /* Subroutine */ int movb_();
    static real rrem;
    static integer kbyt;
    extern integer ksys_(integer *);
    static integer idcal;
    static char calbl[1*7168];
    static integer iarea;
    static char cfile[12], cname[4];
    static integer ibeta[21]	/* was [7][3] */, idate[2], ifact[3], idsen;
    extern /* Subroutine */ int edest_(char *, integer *, ftnlen);
    static integer ntabs, istat;
    extern /* Subroutine */ int sysin_(integer *, integer *);
    static integer icalbl[1792];
    extern /* Subroutine */ int swbyt4_(integer *, integer *), araget_(
	    integer *, integer *, integer *, integer *);
    static integer ispare[3], lbstart, nbstart;

/*  All variables must be declared */
/*     from each line header for IBUF dat */
/*  Input array of calibration constants */
/*     for GMS, since the area calibratio */
/*     block must be accessed locally) */
/*  Area directory buffer (this is mandat */
/*  Band number of data in area */
/*     absolute radiation temperature */
/*  Lookup table containing albedo or */
/*     (defines NUMAREAOPTIONS) */
/*  Global declarations for McIDAS areas */
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

/*  Flag identifying conversion code */
/*  Source pixel size in bytes */
/*  Destination pixel size in bytes */
/*  Flag specifying how to construct ITAB */
/*  Calibration parameters for conve */
/*  Radiance table for GMS-5 */
/*  We use only IVAL to pass back */
/*   the calibration option used */
/*   and rename ITAB because its */
/*   address is passed as an arg */
/*   platform or compiler depend */
/*   too, and might be susceptab */
/*  ID number of calibration table */
/*  byte offset into GMSCAL file(s) */
/*  Logical .AND. function */
/*  Acquire SYSVAL value */
/*  LW file read */
/*  Converts CHAR*4 to INTEGER */
/*  Real MOD function */
/*  Converts GMS-5 TEMP to RAD */
/*  index variable */
/*  index variable */
/*  index variable */
/*  Length of arbitrary block in byt */
/*  End word count of a block */
/*  Input block starting byte count */
/*  Output block starting byte count */
/*  Input index value */
/*  Interpolated data output index v */
/*  First table value */
/*  Second table value */
/*  area number from IDIR array */
/*  spacecraft/sensor ID from IDIR a */
/*  LWI status returned */
/*  Year */
/*  Month */
/*  Day of month */
/*  Hour of day (GMT) */
/*  Minute of hour */
/*    (from first word in file) */
/*  number of tables in the GMSCAL f */
/*  Default GMSCAL table directory */
/*  GMS-5 McIDAS Calibration block */
/*  Calibration ID from data block */
/*    (YY,YY,MM,DD,HH,mm) */
/*  Six-byte date data block was gen */
/*    (1=primary,   2=redundant) */
/*  Sensor selector byte */
/*    (significant if non-zero) */
/*  bad radiance value counter */
/*    (significant if non-zero) */
/*  error flag for GMP_FCO function */
/*  Byte address of CNAME block */
/*    IR bands for converting DN to */
/*    sensor output voltage (scaled */
/*  Calibration constants for the th */
/*    the series expansion for each */
/*  Number of calibration constants */
/*  G constant for each IR band */
/*  V0 constant for each IR band */
/*  C0 constant for each IR band */
/*  Unused spare location in table */
/*    the series expansion to be use */
/*  Number of calibration constants */
/*  Real input index value IV */
/*  Remainder mod 1.0 */
/*    IR bands for converting DN to */
/*    sensor output voltage */
/*  Calibration constants for the th */
/*     (volts/watt/cm**2/sr) */
/*  G constant to be used */
/*     (zero level voltage) */
/*  V0 constant to be used */
/*  C0 constant to be used */
/*    series expansion */
/*  Intermediate "DN" value used in */
/*  Output voltage from series expan */
/*     (watts/cm**2/sr) */
/*  Radiance from voltage and G cons */
/*     W/cm**2/sr to W/etc/cm**-1 */
/*     for the GMS-5 IR bands */
/*  Wave number factor to convert fr */
/*     calibration tables */
/*  Input file containing default */
/*  GMS-5 McIDAS Calibration block */
/*  Table Name in calibration data b */
    /* Parameter adjustments */
    --itab;
    --idir;
    --calb;

    /* Function Body */
    iss = idir[3];
    iarea = idir[33];
    ltab = 0;
    if (idir[3] == 12) {
	ltab = 1;
    } else if (idir[3] == 13) {
	ltab = 3;
    } else if (idir[3] == 82 && *iband == 1) {
	ltab = 1;
    } else if (idir[3] == 82 && *iband == 2) {
	ltab = 3;
    } else if (idir[3] == 82 && *iband == 8) {
	ltab = 3;
    } else if (idir[3] == 83 && *iband == 1) {
	ltab = 4;
    } else if (idir[3] == 83 && *iband == 2) {
	ltab = 5;
    } else if (idir[3] == 83 && *iband == 3) {
	ltab = 6;
    } else if (idir[3] == 83 && *iband == 4) {
	ltab = 7;
    } else if (idir[3] == 83 && *iband == 8) {
	ltab = 5;
    } else {
	edest_("RD_CAL: Unrecognized data band or SS", &c__0, (ftnlen)36);
	ret_val = -1;
	goto L500;
    }
    if (gmsxxgmskb2_1.kopt <= 0 || gmsxxgmskb2_1.kopt > 15) {
	gmsxxgmskb2_1.kopt = ksys_(&c__151);
    }
    if (gmsxxgmskb2_1.kopt <= 0 || gmsxxgmskb2_1.kopt > 15) {
	gmsxxgmskb2_1.kopt = 7;
	if (idir[3] <= 82) {
	    gmsxxgmskb2_1.kopt = 3;
	}
    }
    if (idir[3] <= 82 && gmsxxgmskb2_1.kopt > 3) {
	edest_("Invalid calibration option for SS=", &idir[3], (ftnlen)34);
	edest_("     ...was ", &gmsxxgmskb2_1.kopt, (ftnlen)12);
	gmsxxgmskb2_1.kopt = 3;
	edest_("     ...reset to ", &gmsxxgmskb2_1.kopt, (ftnlen)17);
	edest_("Options > 3 valid for GMS-5 and later only!", &c__0, (ftnlen)
		43);
    }
    debuggmskb2_2.ival = 0;
    idsen = 0;
    if ((gmsxxgmskb2_1.kopt & 12) != 0) {
	araget_(&iarea, &idir[63], &c__7168, icalbl);
	if (icalbl[0] != lit_("GMS5", (ftnlen)4) || icalbl[1] > 90) {
	    edest_("Calibration block is not GMS-5 format.", &c__0, (ftnlen)
		    38);
	    edest_("It cannot be used by the GMS calibration module.", &c__0, 
		    (ftnlen)48);
	    edest_("Will attempt to use GMSCAL file tables instead.", &c__0, (
		    ftnlen)47);
	    goto L200;
	}
	nlen = icalbl[1];
	if (icalbl[2] == lit_("COEF", (ftnlen)4)) {
	    movb_(&c__256, icalbl, calbl, &c__0, (ftnlen)1);
	    for (i__ = 1; i__ <= 60; i__ += 3) {
		i__1 = i__ + 2;
		for (j = i__; j <= i__1; ++j) {
		    for (k = -3; k <= 0; ++k) {
			if (*(unsigned char *)&calbl[(j << 2) + k - 1] < 32 ||
				 *(unsigned char *)&calbl[(j << 2) + k - 1] > 
				126) {
			    *(unsigned char *)&calbl[(j << 2) + k - 1] = '*';
			}
		    }
		}
	    }
	    movb_(&c__256, &icalbl[icalbl[3] / 4], calbl, &c__0, (ftnlen)1);
	    movb_(&c__4, calbl, &idcal, &c__0);
	    movb_(&c__6, calbl + 4, idate, &c__0, (ftnlen)1);
	    iyr = idate[0] / 65536;
	    imo = (idate[0] - (iyr << 16)) / 256;
	    ida = idate[0] - (iyr << 16) - (imo << 8);
	    ihr = idate[1] / 65536 / 256;
	    imn = idate[1] / 65536 - (ihr << 8);
	    movb_(&c__1, calbl + 10, &idsen, &c__0, (ftnlen)1);
	    idsen /= 16777216;
/* shift right 3 bytes */
	    ifact[0] = *(unsigned char *)&calbl[11];
	    movb_(&c__4, calbl + 12, ibeta, &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 16, &ibeta[1], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 20, &ibeta[2], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 24, &ibeta[3], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 28, &ibeta[4], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 32, &ibeta[5], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 36, &ibeta[6], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 40, ig, &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 44, iv0, &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 48, ic0, &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 52, ispare, &c__0, (ftnlen)1);
	    ifact[1] = *(unsigned char *)&calbl[56];
	    movb_(&c__4, calbl + 57, &ibeta[7], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 61, &ibeta[8], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 65, &ibeta[9], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 69, &ibeta[10], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 73, &ibeta[11], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 77, &ibeta[12], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 81, &ibeta[13], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 85, &ig[1], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 89, &iv0[1], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 93, &ic0[1], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 97, &ispare[1], &c__0, (ftnlen)1);
	    ifact[2] = *(unsigned char *)&calbl[101];
	    movb_(&c__4, calbl + 102, &ibeta[14], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 106, &ibeta[15], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 110, &ibeta[16], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 114, &ibeta[17], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 118, &ibeta[18], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 122, &ibeta[19], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 126, &ibeta[20], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 130, &ig[2], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 134, &iv0[2], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 138, &ic0[2], &c__0, (ftnlen)1);
	    movb_(&c__4, calbl + 142, &ispare[2], &c__0, (ftnlen)1);
	    if (*iband > 1 && *iband <= 4) {
		g = (ig[*iband - 2] & 2147483647) * 1e-6f;
		v0 = (iv0[*iband - 2] & 2147483647) * 1e-6f;
		c0 = (ic0[*iband - 2] & 2147483647) * 1e-6f;
		r__1 = (real) ig[*iband - 2];
		g = r_sign(&g, &r__1);
		r__1 = (real) iv0[*iband - 2];
		v0 = r_sign(&v0, &r__1);
		r__1 = (real) ic0[*iband - 2];
		c0 = r_sign(&c0, &r__1);
		ifac = ifact[*iband - 2] + 1;
		if (ifac < 1 || ifac > 7) {
		    ifac = 7;
		}
		for (i__ = 1; i__ <= 3; ++i__) {
		    i__1 = ifac;
		    for (j = 1; j <= i__1; ++j) {
			beta[j + i__ * 7 - 8] = (ibeta[j + i__ * 7 - 8] & 
				2147483647) * 1e-6f;
			r__1 = (real) ibeta[j + i__ * 7 - 8];
			beta[j + i__ * 7 - 8] = r_sign(&beta[j + i__ * 7 - 8],
				 &r__1);
		    }
		}
		for (j = 1; j <= 255; ++j) {
/*  index on S-VISSR DN l */
		    c__ = 255.f - j + c0;
/*  convert to instrument */
		    v = 0.f;
		    i__1 = ifac;
		    for (k = 1; k <= i__1; ++k) {
/*  do the series expansi */
			i__2 = k - 1;
			v += beta[k + (*iband - 1) * 7 - 8] * pow_ri(&c__, &
				i__2);
		    }
		    r__ = (v - v0) / g;
/*  convert to radiance W */
		    r__ = r__ * .5f / wnfctr[*iband - 2];
/*  convert to mW/etc./cm */
		    radgmskb2_1.ktab[j - 1] = (integer) (r__ * 1e3f);
/*     the KTAB table as */
/*  scale by 1000 and put */
		}
		iok = 0;
		for (j = 2; j <= 255; ++j) {
		    if (radgmskb2_1.ktab[j - 1] < 0 || radgmskb2_1.ktab[j - 1]
			     > 190000) {
			++iok;
		    }
		}
	    }
	} else {
	    goto L100;
	}
	if (ltab <= 3) {
	    goto L100;
	}
	if (ltab == 4) {
	    s_copy(cname, "5VIS", (ftnlen)4, (ftnlen)4);
	}
	if (ltab == 5) {
	    s_copy(cname, "5IR1", (ftnlen)4, (ftnlen)4);
	}
	if (ltab == 6) {
	    s_copy(cname, "5IR2", (ftnlen)4, (ftnlen)4);
	}
	if (ltab == 7) {
	    s_copy(cname, "5IR3", (ftnlen)4, (ftnlen)4);
	}
	if (ltab >= 8) {
	    goto L100;
	}
	kbyt = 0;
	kend = nlen / 4;
	if (kend > 22) {
	    edest_("RD_CAL:  Bad directory structure. KEND=", &kend, (ftnlen)
		    39);
	    kend = 22;
	}
	i__1 = kend;
	for (j = 5; j <= i__1; ++j) {
	    if (icalbl[j - 1] == lit_(cname, (ftnlen)4)) {
		kbyt = icalbl[j];
	    }
	}
	if (kbyt != 0) {
	    movb_(&c__1024, &icalbl[kbyt / 4], &itab[1], &c__0);
	    itab[1] = itab[2];
	    itab[256] = itab[255];
	} else {
	    edest_("KBYT cannot be set -- no table exists", &c__0, (ftnlen)37)
		    ;
	    goto L100;
	}
	if (*iband != 1) {
	    goto L80;
	}
	for (i__ = 4; i__ >= 1; --i__) {
	    lbstart = (i__ - 1 << 6) + 1;
	    nbstart = i__ - 1 << 8;
	    for (ixv = 256; ixv >= 1; --ixv) {
		riv = (real) (ixv - 1) / 252.f * 63.f;
		iv = riv;
		v1 = itab[lbstart + iv];
		v2 = itab[lbstart + iv + 1];
		if (iv == 63) {
		    v2 = 1000000;
		}
		rrem = r_mod(&riv, &c_b208);
		itab[nbstart + ixv] = v1 + (v2 - v1) * rrem + .5f;
	    }
	}
L80:
	debuggmskb2_2.ival = 8;
	if (iok > 5 && (gmsxxgmskb2_1.kopt & 15) == 8) {
	    edest_("Too many of the real-time power series radiances are", &
		    c__0, (ftnlen)52);
	    edest_("outside expected limits and are likely to be in error.", &
		    c__0, (ftnlen)54);
	    goto L100;
	}
	if ((gmsxxgmskb2_1.kopt & 4) == 0) {
	    goto L400;
	}
    }
L100:
    if ((gmsxxgmskb2_1.kopt & 4) != 0) {
	if (iss == 83) {
	    if (idsen != 2) {
		gmpfcogmskb2_("A", &jok, (ftnlen)1);
	    }
	    if (idsen == 2) {
		gmpfcogmskb2_("B", &jok, (ftnlen)1);
	    }
	    if (jok == 0) {
		goto L300;
	    }
	    for (j = 1; j <= 256; ++j) {
		r__1 = itab[j] * .001f;
		radgmskb2_1.ktab[j - 1] = gmtradgmskb2_(&r__1, iband) * 1e3f;
	    }
	} else {
	    goto L200;
	}
	debuggmskb2_2.ival = 4;
	goto L400;
    }
L200:
    if ((gmsxxgmskb2_1.kopt & 2) != 0) {
	s_copy(cfile, "GMSCALU", (ftnlen)12, (ftnlen)7);
	istat = lwi_(cfile, &c__0, &c__512, dir, (ftnlen)12);
	swbyt4_(dir, &c__512);
	if (istat != 0 && (gmsxxgmskb2_1.kopt & 1) == 0) {
/* Writing concatenation */
	    i__3[0] = 43, a__1[0] = "ERROR reading calibration tables from f"
		    "ile ";
	    i__3[1] = 12, a__1[1] = cfile;
	    s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)55);
	    edest_(ch__1, &c__0, (ftnlen)55);
	    goto L300;
	}
	ntabs = dir[0];
	ioff = (ltab - 1 << 8) + 512;
	if (ltab > ntabs) {
	    goto L300;
	}
	istat = lwi_(cfile, &ioff, &c__256, &itab[1], (ftnlen)12);
	swbyt4_(&itab[1], &c__256);
	for (j = 1; j <= 256; ++j) {
	    itab[j + 256] = itab[j];
	    itab[j + 512] = itab[j];
	    itab[j + 768] = itab[j];
	}
	if (istat != 0) {
	    goto L300;
	}
	if (iss == 83) {
	    if (idsen != 2) {
		gmpfcogmskb2_("A", &jok, (ftnlen)1);
	    }
	    if (idsen == 2) {
		gmpfcogmskb2_("B", &jok, (ftnlen)1);
	    }
	    if (jok == 0) {
		goto L300;
	    }
	    for (j = 1; j <= 256; ++j) {
		r__1 = itab[j] * .001f;
		radgmskb2_1.ktab[j - 1] = gmtradgmskb2_(&r__1, iband) * 1e3f;
	    }
	}
	debuggmskb2_2.ival = 2;
	goto L400;
    }
L300:
    if ((gmsxxgmskb2_1.kopt & 1) != 0) {
	s_copy(cfile, "GMSCAL", (ftnlen)12, (ftnlen)6);
	istat = lwi_(cfile, &c__0, &c__512, dir, (ftnlen)12);
	swbyt4_(dir, &c__512);
	if (istat != 0) {
/* Writing concatenation */
	    i__3[0] = 43, a__1[0] = "ERROR reading calibration tables from f"
		    "ile ";
	    i__3[1] = 12, a__1[1] = cfile;
	    s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)55);
	    edest_(ch__1, &c__0, (ftnlen)55);
	    ret_val = -1;
	    goto L500;
	}
	ntabs = dir[0];
	ioff = (ltab - 1 << 8) + 512;
	if (ltab > ntabs) {
	    edest_("RD_CAL ERROR -- Unrecognized Table ID=", &ltab, (ftnlen)
		    38);
/* Writing concatenation */
	    i__3[0] = 61, a__1[0] = "The table index is greater than number "
		    "of tables in the file ";
	    i__3[1] = 12, a__1[1] = cfile;
	    s_cat(ch__2, a__1, i__3, &c__2, (ftnlen)73);
	    edest_(ch__2, &c__0, (ftnlen)73);
	    edest_("Check SS and band number for consistency with the table "
		    "ID", &c__0, (ftnlen)58);
	    edest_("                                   SS=", &idir[3], (
		    ftnlen)38);
	    edest_("                                 Band=", iband, (ftnlen)
		    38);
	    ret_val = -1;
	    goto L500;
	}
	istat = lwi_(cfile, &ioff, &c__256, &itab[1], (ftnlen)12);
	swbyt4_(&itab[1], &c__256);
	for (j = 1; j <= 256; ++j) {
	    itab[j + 256] = itab[j];
	    itab[j + 512] = itab[j];
	    itab[j + 768] = itab[j];
	}
	if (istat != 0) {
/* Writing concatenation */
	    i__3[0] = 30, a__1[0] = "File status error reading file";
	    i__3[1] = 12, a__1[1] = cfile;
	    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)42);
	    edest_(ch__3, &istat, (ftnlen)42);
	    ret_val = -1;
	    goto L500;
	}
	if (iss == 83) {
	    if (idsen != 2) {
		gmpfcogmskb2_("A", &jok, (ftnlen)1);
	    }
	    if (idsen == 2) {
		gmpfcogmskb2_("B", &jok, (ftnlen)1);
	    }
	    if (jok == 0) {
		ret_val = -1;
		goto L500;
	    }
	    for (j = 1; j <= 256; ++j) {
		r__1 = itab[j] * .001f;
		radgmskb2_1.ktab[j - 1] = gmtradgmskb2_(&r__1, iband) * 1e3f;
	    }
	}
	debuggmskb2_2.ival = 1;
	goto L400;
    }
    ret_val = -1;
    goto L500;
L400:
    if (debuggmskb2_2.ival == 0) {
	ret_val = -1;
    } else {
	ret_val = 0;
    }
L500:
    sysin_(&c__152, &debuggmskb2_2.ival);
    for (i__ = 1; i__ <= 1024; ++i__) {
	debuggmskb2_2.itabr[i__ - 1] = itab[i__];
    }
    return ret_val;
} /* rdcalgmskb2_ */

