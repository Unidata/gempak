/* kb1msg.f -- translated by f2c (version 20031025).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer itype, jtype, jopt[5], calflg, calarr[313];
} msgcommsgkb1_;

#define msgcommsgkb1_1 msgcommsgkb1_

/* Table of constant values */

static integer c__5 = 5;
static integer c__51 = 51;
static integer c__104 = 104;
static integer c__313 = 313;
static integer c__1252 = 1252;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__4 = 4;

integer kb1inimsg_(char *cin, char *cout, integer *iopt, ftnlen cin_len, 
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
    movw_(&c__5, &iopt[1], msgcommsgkb1_1.jopt);
    msgcommsgkb1_1.itype = 0;
    msgcommsgkb1_1.calflg = 0;
    if (s_cmp(cin, "RAW", (ftnlen)4, (ftnlen)3) == 0 && s_cmp(cout, "BRIT", (
	    ftnlen)4, (ftnlen)4) == 0) {
	msgcommsgkb1_1.itype = 1;
    }
    if (s_cmp(cin, "RAW", (ftnlen)4, (ftnlen)3) == 0 && s_cmp(cout, "RAD ", (
	    ftnlen)4, (ftnlen)4) == 0) {
	msgcommsgkb1_1.itype = 2;
    }
    if (s_cmp(cin, "RAW", (ftnlen)4, (ftnlen)3) == 0 && s_cmp(cout, "REFL", (
	    ftnlen)4, (ftnlen)4) == 0) {
	msgcommsgkb1_1.itype = 3;
    }
    if (s_cmp(cin, "RAW", (ftnlen)4, (ftnlen)3) == 0 && s_cmp(cout, "TEMP", (
	    ftnlen)4, (ftnlen)4) == 0) {
	msgcommsgkb1_1.itype = 4;
    }
    if (msgcommsgkb1_1.itype == 0) {
	goto L900;
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* kb1inimsg_ */

integer kb1calmsg_(integer *pfx, integer *idir, integer *nval, integer *band, 
	shortint *ibuf)
{
    /* Initialized data */

    static real factor[12] = { 21.21f,23.24f,19.77f,0.f,0.f,0.f,0.f,0.f,0.f,
	    0.f,0.f,22.39f };
    static integer this__ = -9999;
    static doublereal c1w3 = 0.;
    static doublereal c2w = 0.;
    static doublereal alpha = 0.;
    static doublereal beta = 0.;
    static doublereal gain = 0.;
    static doublereal offset = 0.;

    /* Format strings */
    static char fmt_1[] = "(6e17.10)";

    /* System generated locals */
    address a__1[2];
    integer ret_val, i__1[2], i__2;
    real r__1;
    char ch__1[116], ch__2[25], ch__3[12], ch__4[27];
    static integer equiv_0[313];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_rsfi(icilist *), do_fio(integer *, char *, ftnlen), e_rsfi(void)
	    , i_nint(real *);
    double sqrt(doublereal), log(doublereal);

    /* Local variables */
    extern /* Subroutine */ int m0sxtrce_(char *, ftnlen);
    static integer i__, bandoffset;
    extern /* Character */ VOID cff_(char *, ftnlen, doublereal *, integer *);
#define buf (equiv_0)
#define cbuf ((char *)equiv_0)
    static integer ides;
    static real refl;
    static char cout[104];
    static integer isou;
    extern /* Subroutine */ int movw_(integer *, integer *, integer *);
    static integer ibrit, itemp;
    static real xtemp;
    extern /* Subroutine */ int araget_(integer *, integer *, integer *, 
	    integer *), mpixel_(integer *, integer *, integer *, shortint *), 
	    gryscl_(real *, integer *);

    /* Fortran I/O blocks */
    static icilist io___13 = { 1, cout, 0, fmt_1, 104, 1 };


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
    --ibuf;
    --idir;
    --pfx;

    /* Function Body */
    if (this__ != idir[33]) {
	this__ = idir[33];
	s_copy(cout, " ", (ftnlen)104, (ftnlen)1);
	if (msgcommsgkb1_1.calflg != 0) {
	    movw_(&c__51, msgcommsgkb1_1.calarr, buf);
	} else {
	    araget_(&idir[33], &idir[63], &c__104, buf);
	}
	if (s_cmp(cbuf, "MSGT", (ftnlen)4, (ftnlen)4) == 0) {
	    if (msgcommsgkb1_1.calflg != 0) {
		movw_(&c__313, msgcommsgkb1_1.calarr, buf);
	    } else {
		araget_(&idir[33], &idir[63], &c__1252, buf);
	    }
	    bandoffset = (*band - 1) * 104 + 5;
	    s_copy(cout, cbuf + (bandoffset - 1), (ftnlen)104, (ftnlen)104);
	} else {
	    s_copy(cout, cbuf, (ftnlen)104, (ftnlen)104);
	}
/* Writing concatenation */
	i__1[0] = 12, a__1[0] = "KBXMSG: CAL=";
	i__1[1] = 104, a__1[1] = cout;
	s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)116);
	m0sxtrce_(ch__1, (ftnlen)116);
/* L1: */
	i__2 = s_rsfi(&io___13);
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = do_fio(&c__1, (char *)&c1w3, (ftnlen)sizeof(doublereal));
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = do_fio(&c__1, (char *)&c2w, (ftnlen)sizeof(doublereal));
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = do_fio(&c__1, (char *)&alpha, (ftnlen)sizeof(doublereal));
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = do_fio(&c__1, (char *)&beta, (ftnlen)sizeof(doublereal));
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = do_fio(&c__1, (char *)&gain, (ftnlen)sizeof(doublereal));
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = do_fio(&c__1, (char *)&offset, (ftnlen)sizeof(doublereal));
	if (i__2 != 0) {
	    goto L999;
	}
	i__2 = e_rsfi();
	if (i__2 != 0) {
	    goto L999;
	}
/* Writing concatenation */
	i__1[0] = 13, a__1[0] = "KBXMSG: GAIN=";
	cff_(ch__3, (ftnlen)12, &gain, &c__4);
	i__1[1] = 12, a__1[1] = ch__3;
	s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)25);
	m0sxtrce_(ch__2, (ftnlen)25);
/* Writing concatenation */
	i__1[0] = 15, a__1[0] = "KBXMSG: OFFSET=";
	cff_(ch__3, (ftnlen)12, &offset, &c__4);
	i__1[1] = 12, a__1[1] = ch__3;
	s_cat(ch__4, a__1, i__1, &c__2, (ftnlen)27);
	m0sxtrce_(ch__4, (ftnlen)27);
	isou = msgcommsgkb1_1.jopt[0];
	ides = msgcommsgkb1_1.jopt[1];
    }
    i__2 = *nval;
    for (i__ = 1; i__ <= i__2; ++i__) {
	itemp = ibuf[i__];
	if (*band < 4 || *band == 12) {
	    if (msgcommsgkb1_1.itype == 4) {
		ibuf[i__] = 0;
	    } else {
		xtemp = (real) itemp * gain + offset;
		if (xtemp <= 0.f) {
		    xtemp = 0.f;
		}
		if (msgcommsgkb1_1.itype == 2) {
		    r__1 = xtemp * 100.f;
		    ibuf[i__] = (shortint) i_nint(&r__1);
		} else if (msgcommsgkb1_1.itype == 3) {
		    refl = xtemp / factor[*band - 1] * 100;
		    if (refl < 0.f) {
			refl = 0.f;
		    }
		    if (refl > 100.f) {
			refl = 100.f;
		    }
		    r__1 = refl * 100;
		    ibuf[i__] = (shortint) i_nint(&r__1);
		} else {
		    refl = xtemp / factor[*band - 1] * 100;
		    if (refl < 0.f) {
			refl = 0.f;
		    }
		    if (refl > 100.f) {
			refl = 100.f;
		    }
		    r__1 = sqrt(refl) * 25.5f;
		    ibuf[i__] = (shortint) i_nint(&r__1);
		}
	    }
	} else {
	    xtemp = gain * itemp + offset;
	    if (xtemp < 0.f) {
		xtemp = 0.f;
	    }
	    if (msgcommsgkb1_1.itype == 2) {
		r__1 = xtemp * 100.f;
		ibuf[i__] = (shortint) i_nint(&r__1);
	    } else if (msgcommsgkb1_1.itype == 3) {
		ibuf[i__] = 0;
	    } else if (msgcommsgkb1_1.itype == 4) {
		if (xtemp > 0.f) {
		    xtemp = (c2w / log(c1w3 / xtemp + 1.f) - beta) / alpha;
		    r__1 = xtemp * 100.f;
		    ibuf[i__] = (shortint) i_nint(&r__1);
		} else {
		    ibuf[i__] = 0;
		}
	    } else {
		if (xtemp > 0.f) {
		    xtemp = (c2w / log(c1w3 / xtemp + 1.f) - beta) / alpha;
		    gryscl_(&xtemp, &ibrit);
		    ibuf[i__] = (shortint) ibrit;
		} else {
		    ibuf[i__] = 255;
		}
	    }
	}
    }
    mpixel_(nval, &isou, &ides, &ibuf[1]);
    ret_val = 0;
    return ret_val;
L999:
    m0sxtrce_("KBXMSG: CAN NOT READ CAL HEADER", (ftnlen)31);
    ret_val = -1;
    return ret_val;
} /* kb1calmsg_ */

#undef cbuf
#undef buf


integer kb1optmsg_(char *cfunc, integer *iin, integer *iout, ftnlen cfunc_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer lit_(char *, ftnlen);
    extern /* Subroutine */ int movw_(integer *, integer *, integer *);

/* external functions */
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

    /* Parameter adjustments */
    --iout;
    --iin;

    /* Function Body */
    ret_val = 0;
    if (s_cmp(cfunc, "KEYS", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[4] <= 3 || iin[4] == 12) {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("REFL", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	} else {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	}
    } else if (s_cmp(cfunc, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[1] <= 3 || iin[1] == 12) {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("REFL", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	    iout[6] = lit_("    ", (ftnlen)4);
	    iout[7] = lit_("MW**", (ftnlen)4);
	    iout[8] = lit_("%   ", (ftnlen)4);
	    iout[9] = lit_("    ", (ftnlen)4);
	    iout[10] = 1;
	    iout[11] = 100;
	    iout[12] = 100;
	    iout[13] = 1;
	} else {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	    iout[6] = lit_("    ", (ftnlen)4);
	    iout[7] = lit_("MW**", (ftnlen)4);
	    iout[8] = lit_("K   ", (ftnlen)4);
	    iout[9] = lit_("    ", (ftnlen)4);
	    iout[10] = 1;
	    iout[11] = 100;
	    iout[12] = 100;
	    iout[13] = 1;
	}
    } else if (s_cmp(cfunc, "CALB", (ftnlen)4, (ftnlen)4) == 0) {
	msgcommsgkb1_1.calflg = 1;
	movw_(&c__313, &iin[1], msgcommsgkb1_1.calarr);
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* kb1optmsg_ */

