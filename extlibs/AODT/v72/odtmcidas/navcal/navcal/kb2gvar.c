/* kb2gvar.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer jtype, isou, ides, jopt[5];
} gvarxxgvarkb2_;

#define gvarxxgvarkb2_1 gvarxxgvarkb2_

struct {
    integer calflg, calarr[128];
} gvrcal2_;

#define gvrcal_1 gvrcal2_

struct {
    char caltyp[4];
} brkpntgvarkb2_;

#define brkpntgvarkb2_1 brkpntgvarkb2_

/* Table of constant values */

static integer c__5 = 5;
static integer c__128 = 128;
static integer c__512 = 512;
static integer c__0 = 0;
static integer c__1 = 1;
static real c_b36 = (float)0.;
static real c_b38 = (float)345.;

integer kb2inigvar_(cin, cout, iopt, cin_len, cout_len)
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
    --iopt;

    /* Function Body */
    movw_(&c__5, &iopt[1], gvarxxgvarkb2_1.jopt);
    gvarxxgvarkb2_1.jtype = 0;
    gvrcal_1.calflg = 0;
    gvarxxgvarkb2_1.isou = iopt[1];
    gvarxxgvarkb2_1.ides = iopt[2];
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "RAD ", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gvarxxgvarkb2_1.jtype = 1;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "ALB ", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gvarxxgvarkb2_1.jtype = 1;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "TEMP", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gvarxxgvarkb2_1.jtype = 2;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "BRIT", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gvarxxgvarkb2_1.jtype = 3;
    }
    if (s_cmp(cin, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && s_cmp(cout, "MODB", (
	    ftnlen)4, (ftnlen)4) == 0) {
	gvarxxgvarkb2_1.jtype = 4;
    }
    if (gvarxxgvarkb2_1.jtype == 0) {
	goto L900;
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* kb2inigvar_ */

integer kb2calgvar_(calb, idir, nval, jband, ibuf)
integer *calb, *idir, *nval, *jband;
shortint *ibuf;
{
    /* Initialized data */

    static integer lastim = -1;
    static integer svboff = 1;
    static integer svaoff = 13;
    static integer svg2off = 10;
    static integer indx = 0;
    static integer list[6]	/* was [3][2] */ = { -1,-1,-1,-1,-1,-1 };
    static integer jshort = 0;
    static integer ivgoff = 9;
    static integer ivboff = 1;
    static integer ivaoff = 25;
    static integer ivg2off = 17;
    static integer svgoff = 5;

    /* System generated locals */
    integer ret_val, i__1, i__2;
    real r__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(), i_nint();
    double pow_dd(), sqrt();

    /* Local variables */
    extern /* Subroutine */ int chkirgvarkb2_();
    static real gain;
    static integer itab[65536]	/* was [32768][2] */;
    static real bias;
    static integer ioff;
    static real valb;
    static integer iarr[128], isat, iraw;
    static real temp;
    extern /* Subroutine */ int movw_();
    static real f;
    extern integer chkcodgvarkb2_();
    static integer i__, iband;
    extern doublereal gvatbbgvarkb2_();
    static real scale;
    static integer intab[256];
    extern /* Subroutine */ int exptabgvarkb2_();
    static integer tblhi, hvcod;
    extern /* Subroutine */ int chkvisgvarkb2_();
    static real vbavg, vgavg;
    static integer tbllo, itemp;
    extern doublereal tmpsclgvarkb2_();
    extern integer grysclgvarkb2_();
    static real vg2avg, g2term, albedo;
    extern /* Subroutine */ int araget_();
    extern integer brkval_();
    static integer igvdet;
    extern /* Subroutine */ int mgvatb_();
    static real newarr[128];
    static logical brktst;
    static real alb, rad;
    static integer igvband;
    static real raw;

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

/* generate the entire lookup table */
/* vs just the region enclosing the data. */
/* Threshold to determine when to */
    /* Parameter adjustments */
    --ibuf;
    --idir;
    --calb;

    /* Function Body */
    isat = idir[3];
    if (isat % 2 == 0 && *jband == 6) {
	iband = 5;
    } else {
	iband = *jband;
    }
    if (*nval <= 100 && jshort == 0) {
	tbllo = 32767;
	tblhi = 0;
	jshort = 1;
	i__1 = *nval;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (ibuf[i__] < 0) {
		itemp = ibuf[i__];
		itemp = (itemp & 65535) / 2 + 1;
	    } else {
		itemp = ibuf[i__] / 2 + 1;
	    }
	    tbllo = min(tbllo,itemp);
	    tblhi = max(tblhi,itemp);
/* L5: */
	}
    } else if (jshort == 1) {
	jshort = -1;
	tbllo = 1;
	tblhi = 32768;
	list[0] = -1;
    } else {
	jshort = -1;
	for (i__ = 1; i__ <= 2; ++i__) {
	    if (idir[33] == list[i__ * 3 - 3] && iband == list[i__ * 3 - 1] &&
		     gvarxxgvarkb2_1.jtype == list[i__ * 3 - 2]) {
		indx = i__;
		goto L100;
	    }
/* L10: */
	}
	tbllo = 1;
	tblhi = 32768;
    }
    if (idir[33] != lastim) {
	lastim = idir[33];
	if (gvrcal_1.calflg != 0) {
	    movw_(&c__128, gvrcal_1.calarr, iarr);
	    hvcod = 1;
	} else if (idir[63] == 0) {
	    hvcod = 0;
	} else {
	    ioff = idir[63];
	    araget_(&idir[33], &ioff, &c__512, iarr);
	    hvcod = 1;
	}
	if (hvcod == 1) {
	    hvcod = chkcodgvarkb2_(iarr);
	}
    }
    for (i__ = 1; i__ <= 2; ++i__) {
	if (list[i__ * 3 - 3] == -1) {
	    indx = i__;
	    goto L16;
	}
/* L15: */
    }
    indx = 1;
L16:
    list[indx * 3 - 3] = idir[33];
    list[indx * 3 - 1] = iband;
    list[indx * 3 - 2] = gvarxxgvarkb2_1.jtype;
    brktst = gvarxxgvarkb2_1.jtype == 4 && s_cmp(brkpntgvarkb2_1.caltyp, 
	    "RAW ", (ftnlen)4, (ftnlen)4) == 0;
    if (brktst) {
	i__1 = tblhi;
	for (i__ = tbllo; i__ <= i__1; ++i__) {
	    r__1 = (real) (i__ - 1) * (float)2.;
	    itab[i__ + (indx << 15) - 32769] = brkval_(&r__1);
/* L36: */
	}
	goto L100;
    }
    if (iband == 1 && isat % 2 == 0 || iband == 19 && isat % 2 == 1) {
	chkvisgvarkb2_(&isat, iarr, newarr, &hvcod);
	vg2avg = (float)0.;
	vgavg = (float)0.;
	vbavg = (float)0.;
	if (iband == 1) {
	    for (itemp = 1; itemp <= 8; ++itemp) {
		vg2avg += newarr[itemp + ivg2off - 2];
		vgavg += newarr[itemp + ivgoff - 2];
		vbavg += newarr[itemp + ivboff - 2];
/* L101: */
	    }
	    vg2avg /= (float)8.;
	    vgavg /= (float)8.;
	    vbavg /= (float)8.;
	    valb = newarr[ivaoff - 1];
	    scale = (float)16.;
	} else {
	    for (itemp = 1; itemp <= 4; ++itemp) {
		vg2avg += newarr[itemp + svg2off - 2];
		vgavg += newarr[itemp + svgoff - 2];
		vbavg += newarr[itemp + svboff - 2];
/* L102: */
	    }
	    vg2avg /= (float)4.;
	    vgavg /= (float)4.;
	    vbavg /= (float)4.;
	    valb = newarr[svaoff - 1];
	    scale = (float)4.;
	}
	brktst = gvarxxgvarkb2_1.jtype == 4 && s_cmp(brkpntgvarkb2_1.caltyp, 
		"ALB ", (ftnlen)4, (ftnlen)4) == 0;
	g2term = (float)1.;
	if (gvarxxgvarkb2_1.jtype == 3 && dabs(vg2avg) < (float)1e-4) {
	    for (i__ = 0; i__ <= 255; ++i__) {
		temp = (i__ + (float).5) / (float)25.5;
		albedo = temp * temp / (float)100.;
		r__1 = scale * (albedo / valb - vbavg - (float)1.) / vgavg;
		iraw = i_nint(&r__1);
		iraw = min(iraw,32767);
		iraw = max(iraw,0);
		intab[i__] = iraw;
/* L38: */
	    }
	    exptabgvarkb2_(intab, &itab[(indx << 15) - 32768], &c__0);
	    goto L100;
	}
	if (*nval > 100) {
	    r__1 = scale * ((float)0. / valb - vbavg - (float)1.) / vgavg;
	    iraw = i_nint(&r__1);
/* Computing MAX */
	    i__1 = 1, i__2 = iraw + 1;
	    tbllo = max(i__1,i__2);
	    r__1 = scale * ((float)1. / valb - vbavg - (float)1.) / vgavg;
	    iraw = i_nint(&r__1);
/* Computing MIN */
	    i__1 = 32768, i__2 = iraw + 1;
	    tblhi = min(i__1,i__2);
	}
	if (gvarxxgvarkb2_1.jtype == 1 || brktst) {
	    i__1 = tblhi;
	    for (i__ = tbllo; i__ <= i__1; ++i__) {
		f = (i__ - 1) / scale;
		if (dabs(vg2avg) > (float)1e-4) {
		    d__1 = (doublereal) f;
		    d__2 = (doublereal) vg2avg;
		    g2term = pow_dd(&d__1, &d__2);
		}
		alb = (g2term + f * vgavg + vbavg) * valb;
		if (alb < (float)0.) {
		    alb = (float)0.;
		}
		if (brktst) {
		    r__1 = alb * (float)100.;
		    itab[i__ + (indx << 15) - 32769] = brkval_(&r__1);
		} else {
		    r__1 = alb * (float)1e3;
		    itab[i__ + (indx << 15) - 32769] = i_nint(&r__1);
		}
/* L40: */
	    }
	} else if (gvarxxgvarkb2_1.jtype >= 3) {
	    brktst = gvarxxgvarkb2_1.jtype == 4 && s_cmp(
		    brkpntgvarkb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 
		    0;
	    i__1 = tblhi;
	    for (i__ = tbllo; i__ <= i__1; ++i__) {
		f = (i__ - 1) / scale;
		if (dabs(vg2avg) > (float)1e-4) {
		    d__1 = (doublereal) f;
		    d__2 = (doublereal) vg2avg;
		    g2term = pow_dd(&d__1, &d__2);
		}
		alb = (g2term + f * vgavg + vbavg) * valb;
		if (alb < (float)0.) {
		    alb = (float)0.;
		}
		if (brktst) {
		    r__1 = sqrt(alb * (float)100.) * (float)25.5;
		    itab[i__ + (indx << 15) - 32769] = brkval_(&r__1);
		} else {
		    r__1 = sqrt(alb * (float)100.) * (float)25.5;
		    itab[i__ + (indx << 15) - 32769] = i_nint(&r__1);
		}
/* L45: */
	    }
	}
    } else {
	chkirgvarkb2_(&isat, &iband, iarr, &gain, &bias, &hvcod);
	if (isat % 2 == 0) {
	    scale = (float)16.;
	    igvband = iband + 20;
	    igvdet = calb[14] / 65536;
	    if (igvdet == 0 && idir[49] != 0) {
		igvdet = 1;
	    } else if (igvdet == 1023 && idir[49] != 0) {
		igvdet = 2;
	    } else if (idir[49] == 0) {
/* NO DOC SECTION */
		igvdet = 0;
	    } else {
		igvdet = 0;
/* UNDEFINED CONFIGURATIO */
	    }
	} else if (isat % 2 == 1) {
	    scale = (float).5;
	    igvband = iband;
	}
	brktst = gvarxxgvarkb2_1.jtype == 4 && s_cmp(brkpntgvarkb2_1.caltyp, 
		"RAD ", (ftnlen)4, (ftnlen)4) == 0;
	if (gvarxxgvarkb2_1.jtype == 3) {
	    for (i__ = 0; i__ <= 255; ++i__) {
		temp = tmpsclgvarkb2_(&i__);
		rad = gvatbbgvarkb2_(&temp, &igvband, &igvdet, &isat, &c__1);
		raw = rad * gain + bias;
		r__1 = raw * scale + (float)1.;
		iraw = i_nint(&r__1);
		iraw = min(iraw,32767);
		iraw = max(iraw,0);
		intab[i__] = iraw;
/* L801: */
	    }
	    exptabgvarkb2_(intab, &itab[(indx << 15) - 32768], &c__1);
	    goto L100;
	}
	if (*nval > 100) {
	    rad = gvatbbgvarkb2_(&c_b36, &igvband, &igvdet, &isat, &c__1);
	    raw = rad * gain + bias;
	    r__1 = raw * scale + (float)1.;
	    iraw = i_nint(&r__1);
/* Computing MAX */
	    i__1 = iraw + 1;
	    tbllo = max(i__1,1);
	    rad = gvatbbgvarkb2_(&c_b38, &igvband, &igvdet, &isat, &c__1);
	    raw = rad * gain + bias;
	    r__1 = raw * scale + (float)1.;
	    iraw = i_nint(&r__1);
/* Computing MIN */
	    i__1 = iraw + 1;
	    tblhi = min(i__1,32768);
	}
	if (gvarxxgvarkb2_1.jtype == 1 || gvarxxgvarkb2_1.jtype == 4 && 
		brktst) {
	    i__1 = tblhi;
	    for (i__ = tbllo; i__ <= i__1; ++i__) {
		f = (real) i__;
		temp = (f - (float)1.) / scale;
		rad = (temp - bias) / gain;
		if (brktst) {
		    itab[i__ + (indx << 15) - 32769] = brkval_(&rad);
		} else {
		    r__1 = rad * (float)1e3;
		    itab[i__ + (indx << 15) - 32769] = i_nint(&r__1);
		}
/* L6: */
	    }
	} else if (gvarxxgvarkb2_1.jtype == 2 || gvarxxgvarkb2_1.jtype == 4) {
	    brktst = gvarxxgvarkb2_1.jtype == 4 && s_cmp(
		    brkpntgvarkb2_1.caltyp, "TEMP", (ftnlen)4, (ftnlen)4) == 
		    0;
	    i__1 = tblhi;
	    for (i__ = tbllo; i__ <= i__1; ++i__) {
		f = (real) i__;
		temp = (f - (float)1.) / scale;
		rad = (temp - bias) / gain;
		temp = gvatbbgvarkb2_(&rad, &igvband, &igvdet, &isat, &c__0);
		if (brktst) {
		    itab[i__ + (indx << 15) - 32769] = brkval_(&temp);
		} else {
		    r__1 = temp * (float)10.;
		    itab[i__ + (indx << 15) - 32769] = i_nint(&r__1);
		}
/* L7: */
	    }
	} else if (gvarxxgvarkb2_1.jtype > 2) {
	    brktst = gvarxxgvarkb2_1.jtype == 4 && s_cmp(
		    brkpntgvarkb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 
		    0;
	    i__1 = tblhi;
	    for (i__ = tbllo; i__ <= i__1; ++i__) {
		f = (real) i__;
		temp = (f - (float)1.) / scale;
		rad = (temp - bias) / gain;
		temp = gvatbbgvarkb2_(&rad, &igvband, &igvdet, &isat, &c__0);
		itemp = grysclgvarkb2_(&temp);
		if (itemp < 0) {
		    itemp = 0;
		}
		if (itemp > 255) {
		    itemp = 255;
		}
		if (brktst) {
		    r__1 = (real) itemp;
		    itab[i__ + (indx << 15) - 32769] = brkval_(&r__1);
		} else {
		    itab[i__ + (indx << 15) - 32769] = itemp;
		}
/* L8: */
	    }
	}
    }
L100:
    mgvatb_(nval, &gvarxxgvarkb2_1.isou, &gvarxxgvarkb2_1.ides, &ibuf[1], &
	    itab[(indx << 15) - 32768]);
    ret_val = 0;
    return ret_val;
} /* kb2calgvar_ */

integer kb2optgvar_(cfunc, iin, iout, cfunc_len)
char *cfunc;
integer *iin, *iout;
ftnlen cfunc_len;
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp();

    /* Local variables */
    static integer isat;
    extern /* Subroutine */ int movw_();
    static integer iband;
    static char cfile[12];
    extern /* Subroutine */ int movcw_();
    extern integer ischar_(), brkset_(), lit_();

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
    iband = iin[4];
    isat = iin[1];
    if (s_cmp(cfunc, "KEYS", (ftnlen)4, (ftnlen)4) == 0) {
	if (iband == 1 && isat % 2 == 0 || iband == 19 && isat % 2 == 1) {
	    iout[1] = 3;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("ALB ", (ftnlen)4);
	    iout[4] = lit_("BRIT", (ftnlen)4);
	    if (ischar_(&iin[38]) == 1) {
		movcw_(&iin[38], cfile, (ftnlen)12);
		if (brkset_(cfile, brkpntgvarkb2_1.caltyp, (ftnlen)12, (
			ftnlen)4) != 0) {
		    ret_val = -3;
		    return ret_val;
		}
	    }
	} else {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	    if (ischar_(&iin[38]) == 1) {
		movcw_(&iin[38], cfile, (ftnlen)12);
		if (brkset_(cfile, brkpntgvarkb2_1.caltyp, (ftnlen)12, (
			ftnlen)4) != 0) {
		    ret_val = -3;
		    return ret_val;
		}
	    }
	}
    } else if (s_cmp(cfunc, "BRKP", (ftnlen)4, (ftnlen)4) == 0) {
	movcw_(&iin[1], cfile, (ftnlen)12);
	if (brkset_(cfile, brkpntgvarkb2_1.caltyp, (ftnlen)12, (ftnlen)4) != 
		0) {
	    ret_val = -3;
	    return ret_val;
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[1] == 1 && iin[2] % 2 == 0 || iin[1] == 19 && iin[2] % 2 == 1)
		 {
	    iout[1] = 3;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("ALB ", (ftnlen)4);
	    iout[4] = lit_("BRIT", (ftnlen)4);
	    iout[5] = lit_("    ", (ftnlen)4);
	    iout[6] = lit_(" %  ", (ftnlen)4);
	    iout[7] = lit_("    ", (ftnlen)4);
	    iout[8] = 1;
	    iout[9] = 10;
	    iout[10] = 1;
	} else {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	    iout[6] = lit_("    ", (ftnlen)4);
	    iout[7] = lit_("MW**", (ftnlen)4);
	    iout[8] = lit_(" K  ", (ftnlen)4);
	    iout[9] = lit_("    ", (ftnlen)4);
	    iout[10] = 1;
	    iout[11] = 1000;
	    iout[12] = 10;
	    iout[13] = 1;
	}
    } else if (s_cmp(cfunc, "CALB", (ftnlen)4, (ftnlen)4) == 0) {
	gvrcal_1.calflg = 1;
	movw_(&c__128, &iin[1], gvrcal_1.calarr);
    }
    ret_val = 0;
    return ret_val;
} /* kb2optgvar_ */

doublereal gvatbbgvarkb2_(val, kch, kdet, isat, switch__)
real *val;
integer *kch, *kdet, *isat, *switch__;
{
    /* Initialized data */

    static real fk1[375]	/* was [25][15] */ = { (float)3756.81,(float)
	    4011.1,(float)4296.87,(float)4681.13,(float)4975.25,(float)
	    5881.41,(float)6787.44,(float)8873.71,(float)12997.94,(float)
	    28629.32,(float)34248.3,(float)43114.3,(float)124235.3,(float)
	    128123.5,(float)135148.2,(float)169167.1,(float)188235.,(float)
	    225794.4,(float)0.,(float)0.,(float)0.,(float)199986.2,(float)
	    38792.39,(float)9737.93,(float)6944.64,(float)3765.12,(float)
	    3981.16,(float)4281.88,(float)4678.91,(float)4962.59,(float)
	    5860.42,(float)6770.32,(float)8958.91,(float)12965.93,(float)
	    28398.28,(float)34201.34,(float)42525.14,(float)124057.4,(float)
	    128011.4,(float)134849.7,(float)167814.2,(float)188801.2,(float)
	    225856.5,(float)0.,(float)0.,(float)0.,(float)198807.8,(float)
	    38732.41,(float)9717.21,(float)6899.47,(float)3730.5,(float)
	    4003.9,(float)4312.4,(float)4661.6,(float)4973.4,(float)5869.8,(
	    float)6816.1,(float)8940.4,(float)12973.,(float)28708.,(float)
	    34401.,(float)43086.,(float)124680.,(float)128820.,(float)135320.,
	    (float)168530.,(float)188620.,(float)224870.,(float)4.337e7,(
	    float)0.,(float)4.337e7,(float)198410.,(float)39086.,(float)
	    9774.4,(float)6828.6,(float)3765.61,(float)3992.81,(float)4303.85,
	    (float)4680.65,(float)4956.33,(float)5858.85,(float)6866.75,(
	    float)8939.46,(float)13026.87,(float)28611.34,(float)34527.,(
	    float)43236.68,(float)124987.94,(float)128845.45,(float)134487.5,(
	    float)169502.47,(float)188726.92,(float)225757.77,(float)0.,(
	    float)0.,(float)0.,(float)200178.17,(float)38788.66,(float)
	    9653.43,(float)6877.84,(float)3777.8,(float)4008.6,(float)4308.5,(
	    float)4704.1,(float)5013.4,(float)5864.5,(float)6907.1,(float)
	    9038.8,(float)12972.,(float)28931.,(float)34531.,(float)43340.,(
	    float)124920.,(float)128220.,(float)135350.,(float)169810.,(float)
	    189540.,(float)225380.,(float)0.,(float)0.,(float)0.,(float)
	    200960.,(float)43702.,(float)9685.9,(float)5047.1,(float)3756.81,(
	    float)4011.1,(float)4296.87,(float)4681.13,(float)4975.25,(float)
	    5881.41,(float)6787.44,(float)8873.71,(float)12997.94,(float)
	    28629.32,(float)34248.3,(float)43114.3,(float)124235.3,(float)
	    128123.5,(float)135148.2,(float)169167.1,(float)188235.,(float)
	    225794.4,(float)0.,(float)0.,(float)0.,(float)199058.3,(float)
	    38761.57,(float)9713.928,(float)6985.63,(float)3765.12,(float)
	    3981.16,(float)4281.88,(float)4678.91,(float)4962.59,(float)
	    5860.42,(float)6770.32,(float)8958.91,(float)12965.93,(float)
	    28398.28,(float)34201.34,(float)42525.14,(float)124057.4,(float)
	    128011.4,(float)134849.7,(float)167814.2,(float)188801.2,(float)
	    225856.5,(float)0.,(float)0.,(float)0.,(float)198701.1,(float)
	    38754.5,(float)9722.976,(float)6909.796,(float)3730.5,(float)
	    4003.9,(float)4312.4,(float)4661.6,(float)4973.4,(float)5869.8,(
	    float)6816.1,(float)8940.4,(float)12973.,(float)28708.,(float)
	    34401.,(float)43086.,(float)124680.,(float)128820.,(float)135320.,
	    (float)168530.,(float)188620.,(float)224870.,(float)4.337e7,(
	    float)0.,(float)4.337e7,(float)198189.4,(float)39100.85,(float)
	    9770.261,(float)6832.162,(float)3765.61,(float)3992.81,(float)
	    4303.85,(float)4680.65,(float)4956.33,(float)5858.85,(float)
	    6866.75,(float)8939.46,(float)13026.87,(float)28611.34,(float)
	    34527.,(float)43236.68,(float)124987.94,(float)128845.45,(float)
	    134487.5,(float)169502.47,(float)188726.92,(float)225757.77,(
	    float)0.,(float)0.,(float)0.,(float)200178.17,(float)38788.66,(
	    float)9653.43,(float)6877.84,(float)3777.8,(float)4008.6,(float)
	    4308.5,(float)4704.1,(float)5013.4,(float)5864.5,(float)6907.1,(
	    float)9038.8,(float)12972.,(float)28931.,(float)34531.,(float)
	    43340.,(float)124920.,(float)128220.,(float)135350.,(float)
	    169810.,(float)189540.,(float)225380.,(float)0.,(float)0.,(float)
	    0.,(float)200960.,(float)43699.,(float)9685.9,(float)5048.5,(
	    float)3756.81,(float)4011.1,(float)4296.87,(float)4681.13,(float)
	    4975.25,(float)5881.41,(float)6787.44,(float)8873.71,(float)
	    12997.94,(float)28629.32,(float)34248.3,(float)43114.3,(float)
	    124235.3,(float)128123.5,(float)135148.2,(float)169167.1,(float)
	    188235.,(float)225794.4,(float)0.,(float)0.,(float)0.,(float)
	    199504.7,(float)38792.39,(float)9747.653,(float)6984.128,(float)
	    3765.12,(float)3981.16,(float)4281.88,(float)4678.91,(float)
	    4962.59,(float)5860.42,(float)6770.32,(float)8958.91,(float)
	    12965.93,(float)28398.28,(float)34201.34,(float)42525.14,(float)
	    124057.4,(float)128011.4,(float)134849.7,(float)167814.2,(float)
	    188801.2,(float)225856.5,(float)0.,(float)0.,(float)0.,(float)
	    198701.1,(float)38732.41,(float)9713.304,(float)6911.536,(float)
	    3730.5,(float)4003.9,(float)4312.4,(float)4661.6,(float)4973.4,(
	    float)5869.8,(float)6816.1,(float)8940.4,(float)12973.,(float)
	    28708.,(float)34401.,(float)43086.,(float)124680.,(float)128820.,(
	    float)135320.,(float)168530.,(float)188620.,(float)224870.,(float)
	    4.337e7,(float)0.,(float)4.337e7,(float)198189.4,(float)39086.,(
	    float)9766.73,(float)6832.463,(float)3765.61,(float)3992.81,(
	    float)4303.85,(float)4680.65,(float)4956.33,(float)5858.85,(float)
	    6866.75,(float)8939.46,(float)13026.87,(float)28611.34,(float)
	    34527.,(float)43236.68,(float)124987.94,(float)128845.45,(float)
	    134487.5,(float)169502.47,(float)188726.92,(float)225757.77,(
	    float)0.,(float)0.,(float)0.,(float)200178.17,(float)38788.66,(
	    float)9653.43,(float)6877.84,(float)3777.8,(float)4008.6,(float)
	    4308.5,(float)4704.1,(float)5013.4,(float)5864.5,(float)6907.1,(
	    float)9038.8,(float)12972.,(float)28931.,(float)34531.,(float)
	    43340.,(float)124920.,(float)128220.,(float)135350.,(float)
	    169810.,(float)189540.,(float)225380.,(float)0.,(float)0.,(float)
	    0.,(float)200960.,(float)43731.,(float)9685.9,(float)5048.5 };
    static real fk2[375]	/* was [25][15] */ = { (float)979.4,(float)
	    1001.01,(float)1024.24,(float)1053.91,(float)1075.53,(float)
	    1137.22,(float)1192.85,(float)1304.33,(float)1481.3,(float)
	    1927.33,(float)2045.96,(float)2209.15,(float)3143.64,(float)
	    3176.1,(float)3233.12,(float)3484.36,(float)3610.65,(float)
	    3836.39,(float)0.,(float)0.,(float)0.,(float)3684.27,(float)
	    2132.72,(float)1345.37,(float)1201.99,(float)980.12,(float)998.52,
	    (float)1023.05,(float)1053.74,(float)1074.62,(float)1135.87,(
	    float)1191.85,(float)1308.49,(float)1480.08,(float)1922.13,(float)
	    2045.03,(float)2199.04,(float)3142.14,(float)3175.18,(float)
	    3230.74,(float)3475.05,(float)3614.26,(float)3836.74,(float)0.,(
	    float)0.,(float)0.,(float)3677.02,(float)2131.62,(float)1344.41,(
	    float)1199.38,(float)977.1,(float)1000.4,(float)1025.5,(float)
	    1052.4,(float)1075.4,(float)1136.5,(float)1194.5,(float)1307.6,(
	    float)1480.4,(float)1929.1,(float)2049.,(float)2208.7,(float)
	    3147.4,(float)3181.8,(float)3234.5,(float)3480.,(float)3613.1,(
	    float)3831.1,(float)22135.,(float)0.,(float)22135.,(float)3674.5,(
	    float)2138.1,(float)1347.,(float)1195.3,(float)980.16,(float)
	    999.49,(float)1024.8,(float)1053.87,(float)1074.17,(float)1135.77,
	    (float)1197.48,(float)1307.54,(float)1482.4,(float)1926.92,(float)
	    2051.5,(float)2211.24,(float)3149.98,(float)3182.06,(float)
	    3227.84,(float)3486.66,(float)3613.79,(float)3836.18,(float)0.,(
	    float)0.,(float)0.,(float)3685.45,(float)2132.65,(float)1341.46,(
	    float)1198.13,(float)981.21,(float)1000.8,(float)1025.2,(float)
	    1055.6,(float)1078.3,(float)1136.1,(float)1199.8,(float)1312.4,(
	    float)1480.3,(float)1934.,(float)2051.6,(float)2213.,(float)
	    3149.4,(float)3176.9,(float)3234.7,(float)3488.8,(float)3618.9,(
	    float)3834.,(float)0.,(float)0.,(float)0.,(float)3690.2,(float)
	    2219.1,(float)1343.,(float)1080.7,(float)979.4,(float)1001.01,(
	    float)1024.24,(float)1053.91,(float)1075.53,(float)1137.22,(float)
	    1192.85,(float)1304.33,(float)1481.3,(float)1927.33,(float)
	    2045.96,(float)2209.15,(float)3143.64,(float)3176.1,(float)
	    3233.12,(float)3484.36,(float)3610.65,(float)3836.39,(float)0.,(
	    float)0.,(float)0.,(float)3678.679,(float)2132.221,(float)
	    1344.302,(float)1204.39,(float)980.12,(float)998.52,(float)
	    1023.05,(float)1053.74,(float)1074.62,(float)1135.87,(float)
	    1191.85,(float)1308.49,(float)1480.08,(float)1922.13,(float)
	    2045.03,(float)2199.04,(float)3142.14,(float)3175.18,(float)
	    3230.74,(float)3475.05,(float)3614.26,(float)3836.74,(float)0.,(
	    float)0.,(float)0.,(float)3676.477,(float)2132.092,(float)
	    1344.719,(float)1200.015,(float)977.1,(float)1000.4,(float)1025.5,
	    (float)1052.4,(float)1075.4,(float)1136.5,(float)1194.5,(float)
	    1307.6,(float)1480.4,(float)1929.1,(float)2049.,(float)2208.7,(
	    float)3147.4,(float)3181.8,(float)3234.5,(float)3480.,(float)
	    3613.1,(float)3831.1,(float)22135.,(float)0.,(float)22135.,(float)
	    3673.318,(float)2138.424,(float)1346.895,(float)1195.504,(float)
	    980.16,(float)999.49,(float)1024.8,(float)1053.87,(float)1074.17,(
	    float)1135.77,(float)1197.48,(float)1307.54,(float)1482.4,(float)
	    1926.92,(float)2051.5,(float)2211.24,(float)3149.98,(float)
	    3182.06,(float)3227.84,(float)3486.66,(float)3613.79,(float)
	    3836.18,(float)0.,(float)0.,(float)0.,(float)3685.45,(float)
	    2132.65,(float)1341.46,(float)1198.13,(float)981.21,(float)1000.8,
	    (float)1025.2,(float)1055.6,(float)1078.3,(float)1136.1,(float)
	    1199.8,(float)1312.4,(float)1480.3,(float)1934.,(float)2051.6,(
	    float)2213.,(float)3149.4,(float)3176.9,(float)3234.7,(float)
	    3488.8,(float)3618.9,(float)3834.,(float)0.,(float)0.,(float)0.,(
	    float)3690.2,(float)2219.1,(float)1343.,(float)1080.8,(float)
	    979.4,(float)1001.01,(float)1024.24,(float)1053.91,(float)1075.53,
	    (float)1137.22,(float)1192.85,(float)1304.33,(float)1481.3,(float)
	    1927.33,(float)2045.96,(float)2209.15,(float)3143.64,(float)
	    3176.1,(float)3233.12,(float)3484.36,(float)3610.65,(float)
	    3836.39,(float)0.,(float)0.,(float)0.,(float)3681.427,(float)
	    2132.72,(float)1345.856,(float)1204.303,(float)980.12,(float)
	    998.52,(float)1023.05,(float)1053.74,(float)1074.62,(float)
	    1135.87,(float)1191.85,(float)1308.49,(float)1480.08,(float)
	    1922.13,(float)2045.03,(float)2199.04,(float)3142.14,(float)
	    3175.18,(float)3230.74,(float)3475.05,(float)3614.26,(float)
	    3836.74,(float)0.,(float)0.,(float)0.,(float)3676.477,(float)
	    2131.62,(float)1344.273,(float)1200.116,(float)977.1,(float)
	    1000.4,(float)1025.5,(float)1052.4,(float)1075.4,(float)1136.5,(
	    float)1194.5,(float)1307.6,(float)1480.4,(float)1929.1,(float)
	    2049.,(float)2208.7,(float)3147.4,(float)3181.8,(float)3234.5,(
	    float)3480.,(float)3613.1,(float)3831.1,(float)22135.,(float)0.,(
	    float)22135.,(float)3673.318,(float)2138.1,(float)1346.733,(float)
	    1195.522,(float)980.16,(float)999.49,(float)1024.8,(float)1053.87,
	    (float)1074.17,(float)1135.77,(float)1197.48,(float)1307.54,(
	    float)1482.4,(float)1926.92,(float)2051.5,(float)2211.24,(float)
	    3149.98,(float)3182.06,(float)3227.84,(float)3486.66,(float)
	    3613.79,(float)3836.18,(float)0.,(float)0.,(float)0.,(float)
	    3685.45,(float)2132.65,(float)1341.46,(float)1198.13,(float)
	    981.21,(float)1000.8,(float)1025.2,(float)1055.6,(float)1078.3,(
	    float)1136.1,(float)1199.8,(float)1312.4,(float)1480.3,(float)
	    1934.,(float)2051.6,(float)2213.,(float)3149.4,(float)3176.9,(
	    float)3234.7,(float)3488.8,(float)3618.9,(float)3834.,(float)0.,(
	    float)0.,(float)0.,(float)3690.2,(float)2219.6,(float)1343.,(
	    float)1080.8 };
    static real tc[750]	/* was [2][25][15] */ = { (float).0123,(float).9999,(
	    float).0133,(float).9999,(float).0186,(float).9999,(float).015,(
	    float).9999,(float).0165,(float).9999,(float).0474,(float).9998,(
	    float).1318,(float).9995,(float).12,(float).9996,(float).0426,(
	    float).9999,(float).1505,(float).9996,(float).2743,(float).9993,(
	    float).1447,(float).9997,(float).0224,(float)1.,(float).022,(
	    float)1.,(float).0217,(float)1.,(float).0579,(float).9999,(float)
	    .0623,(float).9999,(float).3675,(float).9995,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float).6357,(float).9991,(
	    float).606,(float).9986,(float).3735,(float).9987,(float).2217,(
	    float).9992,(float).0099,(float)1.,(float).0119,(float).9999,(
	    float).0122,(float).9999,(float).0119,(float).9999,(float).0135,(
	    float).9999,(float).044,(float).9998,(float).1345,(float).9995,(
	    float).1193,(float).9996,(float).0407,(float).9999,(float).1438,(
	    float).9996,(float).2762,(float).9993,(float).137,(float).9997,(
	    float).0189,(float)1.,(float).0198,(float)1.,(float).0191,(float)
	    1.,(float).0531,(float).9999,(float).0612,(float).9999,(float)
	    .3042,(float).9996,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float).5864,(float).9992,(float).4841,(float).9989,(
	    float).3622,(float).9988,(float).2014,(float).9992,(float).00988,(
	    float).99995,(float).01196,(float).99994,(float).01245,(float)
	    .99994,(float).01245,(float).99995,(float).01366,(float).99994,(
	    float).04311,(float).99983,(float).13973,(float).99947,(float)
	    .11707,(float).99959,(float).03979,(float).99988,(float).14968,(
	    float).99962,(float).27603,(float).99933,(float).13049,(float)
	    .9997,(float).02008,(float).99997,(float).01834,(float).99997,(
	    float).02017,(float).99997,(float).05292,(float).99992,(float)
	    .0533,(float).99992,(float).28683,(float).99961,(float)0.,(float)
	    1.,(float)0.,(float)0.,(float)0.,(float)1.,(float).62226,(float)
	    .99912,(float).61438,(float).99857,(float).27791,(float).99905,(
	    float).21145,(float).99919,(float).0096,(float)1.,(float).0121,(
	    float).9999,(float).0123,(float).9999,(float).0119,(float).9999,(
	    float).0132,(float).9999,(float).0449,(float).9998,(float).1299,(
	    float).9995,(float).118,(float).9996,(float).0386,(float).9999,(
	    float).1509,(float).9996,(float).2738,(float).9993,(float).1345,(
	    float).9997,(float).019,(float)1.,(float).0194,(float)1.,(float)
	    .0201,(float)1.,(float).0488,(float).9999,(float).0542,(float)
	    .9999,(float).2916,(float).9996,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float).6286,(float).9991,(float)
	    .5934,(float).9986,(float).3828,(float).9987,(float).2026,(float)
	    .9992,(float).0101,(float).99995,(float).01252,(float).99994,(
	    float).01229,(float).99994,(float).01189,(float).99995,(float)
	    .01264,(float).99995,(float).04189,(float).99983,(float).13474,(
	    float).99949,(float).12341,(float).99957,(float).03844,(float)
	    .99988,(float).15764,(float).9996,(float).2742,(float).99934,(
	    float).13683,(float).99969,(float).02124,(float).99996,(float)
	    .0178,(float).99997,(float).02037,(float).99997,(float).04933,(
	    float).99993,(float).05386,(float).99992,(float).28872,(float)
	    .99961,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float).69703,(float).99902,(float)5.08315,(float).98872,(
	    float).37554,(float).99872,(float).09537,(float).9996,(float)
	    .0123,(float).9999,(float).0133,(float).9999,(float).0186,(float)
	    .9999,(float).015,(float).9999,(float).0165,(float).9999,(float)
	    .0474,(float).9998,(float).1318,(float).9995,(float).12,(float)
	    .9996,(float).0426,(float).9999,(float).1505,(float).9996,(float)
	    .2743,(float).9993,(float).1447,(float).9997,(float).0224,(float)
	    1.,(float).022,(float)1.,(float).0217,(float)1.,(float).0579,(
	    float).9999,(float).0623,(float).9999,(float).3675,(float).9995,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    .577653,(float).99849,(float).593062,(float).998584,(float)
	    .322176,(float).998731,(float).422077,(float).998831,(float).0099,
	    (float)1.,(float).0119,(float).9999,(float).0122,(float).9999,(
	    float).0119,(float).9999,(float).0135,(float).9999,(float).044,(
	    float).9998,(float).1345,(float).9995,(float).1193,(float).9996,(
	    float).0407,(float).9999,(float).1438,(float).9996,(float).2762,(
	    float).9993,(float).137,(float).9997,(float).0189,(float)1.,(
	    float).0198,(float)1.,(float).0191,(float)1.,(float).0531,(float)
	    .9999,(float).0612,(float).9999,(float).3042,(float).9996,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    .579362,(float).999059,(float).492486,(float).998925,(float)
	    .384301,(float).998709,(float).30271,(float).99906,(float).00988,(
	    float).99995,(float).01196,(float).99994,(float).01245,(float)
	    .99994,(float).01245,(float).99995,(float).01366,(float).99994,(
	    float).04311,(float).99983,(float).13973,(float).99947,(float)
	    .11707,(float).99959,(float).03979,(float).99988,(float).14968,(
	    float).99962,(float).27603,(float).99933,(float).13049,(float)
	    .9997,(float).02008,(float).99997,(float).01834,(float).99997,(
	    float).02017,(float).99997,(float).05292,(float).99992,(float)
	    .0533,(float).99992,(float).28683,(float).99961,(float)0.,(float)
	    1.,(float)0.,(float)0.,(float)0.,(float)1.,(float).605178,(float)
	    .9989,(float).615675,(float).998601,(float).271027,(float).999034,
	    (float).264813,(float).999092,(float).0096,(float)1.,(float).0121,
	    (float).9999,(float).0123,(float).9999,(float).0119,(float).9999,(
	    float).0132,(float).9999,(float).0449,(float).9998,(float).1299,(
	    float).9995,(float).118,(float).9996,(float).0386,(float).9999,(
	    float).1509,(float).9996,(float).2738,(float).9993,(float).1345,(
	    float).9997,(float).019,(float)1.,(float).0194,(float)1.,(float)
	    .0201,(float)1.,(float).0488,(float).9999,(float).0542,(float)
	    .9999,(float).2916,(float).9996,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float).6286,(float).9991,(float)
	    .5934,(float).9986,(float).3828,(float).9987,(float).2026,(float)
	    .9992,(float).0101,(float).99995,(float).01252,(float).99994,(
	    float).01229,(float).99994,(float).01189,(float).99995,(float)
	    .01264,(float).99995,(float).04189,(float).99983,(float).13474,(
	    float).99949,(float).12341,(float).99957,(float).03844,(float)
	    .99988,(float).15764,(float).9996,(float).2742,(float).99934,(
	    float).13683,(float).99969,(float).02124,(float).99996,(float)
	    .0178,(float).99997,(float).02037,(float).99997,(float).04933,(
	    float).99993,(float).05386,(float).99992,(float).28872,(float)
	    .99961,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float).69707,(float).99902,(float)5.08462,(float).98871,(
	    float).37566,(float).99872,(float).08876,(float).99962,(float)
	    .0123,(float).9999,(float).0133,(float).9999,(float).0186,(float)
	    .9999,(float).015,(float).9999,(float).0165,(float).9999,(float)
	    .0474,(float).9998,(float).1318,(float).9995,(float).12,(float)
	    .9996,(float).0426,(float).9999,(float).1505,(float).9996,(float)
	    .2743,(float).9993,(float).1447,(float).9997,(float).0224,(float)
	    1.,(float).022,(float)1.,(float).0217,(float)1.,(float).0579,(
	    float).9999,(float).0623,(float).9999,(float).3675,(float).9995,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    .580963,(float).99847,(float).606,(float).9986,(float).351435,(
	    float).998709,(float).466368,(float).998745,(float).0099,(float)
	    1.,(float).0119,(float).9999,(float).0122,(float).9999,(float)
	    .0119,(float).9999,(float).0135,(float).9999,(float).044,(float)
	    .9998,(float).1345,(float).9995,(float).1193,(float).9996,(float)
	    .0407,(float).9999,(float).1438,(float).9996,(float).2762,(float)
	    .9993,(float).137,(float).9997,(float).0189,(float)1.,(float)
	    .0198,(float)1.,(float).0191,(float)1.,(float).0531,(float).9999,(
	    float).0612,(float).9999,(float).3042,(float).9996,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float).579362,(
	    float).999059,(float).4841,(float).9989,(float).363241,(float)
	    .99873,(float).306547,(float).999053,(float).00988,(float).99995,(
	    float).01196,(float).99994,(float).01245,(float).99994,(float)
	    .01245,(float).99995,(float).01366,(float).99994,(float).04311,(
	    float).99983,(float).13973,(float).99947,(float).11707,(float)
	    .99959,(float).03979,(float).99988,(float).14968,(float).99962,(
	    float).27603,(float).99933,(float).13049,(float).9997,(float)
	    .02008,(float).99997,(float).01834,(float).99997,(float).02017,(
	    float).99997,(float).05292,(float).99992,(float).0533,(float)
	    .99992,(float).28683,(float).99961,(float)0.,(float)1.,(float)0.,(
	    float)0.,(float)0.,(float)1.,(float).605178,(float).9989,(float)
	    .61438,(float).99857,(float).270378,(float).999032,(float).260331,
	    (float).999105,(float).0096,(float)1.,(float).0121,(float).9999,(
	    float).0123,(float).9999,(float).0119,(float).9999,(float).0132,(
	    float).9999,(float).0449,(float).9998,(float).1299,(float).9995,(
	    float).118,(float).9996,(float).0386,(float).9999,(float).1509,(
	    float).9996,(float).2738,(float).9993,(float).1345,(float).9997,(
	    float).019,(float)1.,(float).0194,(float)1.,(float).0201,(float)
	    1.,(float).0488,(float).9999,(float).0542,(float).9999,(float)
	    .2916,(float).9996,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float).6286,(float).9991,(float).5934,(float).9986,(
	    float).3828,(float).9987,(float).2026,(float).9992,(float).0101,(
	    float).99995,(float).01252,(float).99994,(float).01229,(float)
	    .99994,(float).01189,(float).99995,(float).01264,(float).99995,(
	    float).04189,(float).99983,(float).13474,(float).99949,(float)
	    .12341,(float).99957,(float).03844,(float).99988,(float).15764,(
	    float).9996,(float).2742,(float).99934,(float).13683,(float)
	    .99969,(float).02124,(float).99996,(float).0178,(float).99997,(
	    float).02037,(float).99997,(float).04933,(float).99993,(float)
	    .05386,(float).99992,(float).28872,(float).99961,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)0.,(float).69707,(float)
	    .99902,(float)5.08801,(float).9887,(float).37566,(float).99872,(
	    float).08876,(float).99962 };

    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double log(), exp();

    /* Local variables */
    static integer kkch;
    static real expn, tt;
    static integer satidx;

    kkch = *kch;
    if (*isat < 72) {
	satidx = 1;
/* goes-8 */
    } else if (*isat < 74) {
	satidx = 2;
/* goes-9 */
    } else if (*isat < 76) {
	satidx = 3;
/* goes-10 */
    } else if (*isat < 78) {
	satidx = 4;
/* goes-11 */
    } else if (*isat < 80) {
	satidx = 5;
/* goes-12 */
    } else {
	satidx = 6;
/* goes-13 */
    }
    if (satidx > 5) {
	satidx = 5;
    }
/* no goes-13 constants yet */
    if (*kdet < 0 || *kdet > 2) {
	*kdet = 0;
    }
    satidx += *kdet * 5;
    ret_val = (float)0.;
    if (*val <= (float)0.) {
	return ret_val;
    }
    if (*switch__ == 0) {
	expn = fk1[kkch + satidx * 25 - 26] / *val + (float)1.;
	tt = fk2[kkch + satidx * 25 - 26] / log(expn);
	ret_val = (tt - tc[(kkch + satidx * 25 << 1) - 52]) / tc[(kkch + 
		satidx * 25 << 1) - 51];
    } else {
	tt = tc[(kkch + satidx * 25 << 1) - 52] + tc[(kkch + satidx * 25 << 1)
		 - 51] * *val;
	expn = exp(fk2[kkch + satidx * 25 - 26] / tt) - (float)1.;
	ret_val = fk1[kkch + satidx * 25 - 26] / expn;
    }
    return ret_val;
} /* gvatbbgvarkb2_ */

integer grysclgvarkb2_(tempk)
real *tempk;
{
    /* Initialized data */

    static integer con1 = 418;
    static integer con2 = 660;
    static real tlim = (float)242.;

    /* System generated locals */
    integer ret_val, i__1;

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
} /* grysclgvarkb2_ */

doublereal tmpsclgvarkb2_(ibrit)
integer *ibrit;
{
    /* Initialized data */

    static real con1 = (float)418.;
    static real con2 = (float)660.;
    static integer ilim = 176;

    /* System generated locals */
    real ret_val;

    if (*ibrit > ilim) {
	ret_val = con1 - *ibrit;
    } else {
	ret_val = (con2 - *ibrit) / (float)2.;
    }
    return ret_val;
} /* tmpsclgvarkb2_ */

doublereal frmtgvarkb2_(value)
integer *value;
{
    /* Initialized data */

    static integer convrt = 2147483647;

    /* System generated locals */
    real ret_val;
    static real equiv_0[1];

    /* Local variables */
#define temp ((integer *)equiv_0)
    extern /* Subroutine */ int swbyt4_(), fltcon_();
#define meh (equiv_0)

    *temp = *value;
    if (*temp < 0) {
	*temp = (*temp ^ convrt) + 1;
    }
    swbyt4_(meh, &c__1);
    fltcon_(meh, &c__1);
    ret_val = *meh;
    return ret_val;
} /* frmtgvarkb2_ */

#undef meh
#undef temp


/* Subroutine */ int chkvisgvarkb2_(isat, codarr, visarr, hvcod)
integer *isat, *codarr;
real *visarr;
integer *hvcod;
{
    /* Initialized data */

    static real ivbias[8] = { (float)-15.4116,(float)-15.3044,(float)-15.389,(
	    float)-15.2684,(float)-15.3111,(float)-15.273,(float)-15.3534,(
	    float)-15.33 };
    static real ivgain[8] = { (float).552808,(float).550187,(float).553975,(
	    float).550833,(float).550946,(float).55219,(float).550459,(float)
	    .550728 };
    static real iv2gain[8] = { (float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)0. };
    static real ivalb = (float).00192979;
    static real svbias[4] = { (float)-60.33,(float)-60.16,(float)-61.77,(
	    float)-61.5 };
    static real svgain[4] = { (float).0648253,(float).0652221,(float).0656024,
	    (float).0664202 };
    static real sv2gain[4] = { (float)0.,(float)0.,(float)0.,(float)0. };
    static real svalb = (float).00220062;
    static real mxdf = (float).001;

    /* System generated locals */
    real r__1;

    /* Local variables */
    static integer iflag;
    extern doublereal pctdifgvarkb2_();
    static integer itemp, codpos, newpos;
    extern doublereal frmtgvarkb2_();

    /* Parameter adjustments */
    --visarr;
    --codarr;

    /* Function Body */
    iflag = 0;
    newpos = 1;
    codpos = 1;
    if (*isat % 2 == 0) {
	for (itemp = 1; itemp <= 8; ++itemp) {
	    r__1 = frmtgvarkb2_(&codarr[codpos]);
	    if (pctdifgvarkb2_(&ivbias[itemp - 1], &r__1) > mxdf && *hvcod == 
		    1) {
		visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
		iflag = 1;
	    } else {
		visarr[newpos] = ivbias[itemp - 1];
	    }
	    ++newpos;
	    ++codpos;
/* L10: */
	}
	for (itemp = 1; itemp <= 8; ++itemp) {
	    r__1 = frmtgvarkb2_(&codarr[codpos]);
	    if (pctdifgvarkb2_(&ivgain[itemp - 1], &r__1) > mxdf && *hvcod == 
		    1) {
		visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
		iflag = 1;
	    } else {
		visarr[newpos] = ivgain[itemp - 1];
	    }
	    ++newpos;
	    ++codpos;
/* L20: */
	}
	for (itemp = 1; itemp <= 8; ++itemp) {
	    r__1 = frmtgvarkb2_(&codarr[codpos]);
	    if (pctdifgvarkb2_(&iv2gain[itemp - 1], &r__1) > mxdf && *hvcod ==
		     1) {
		visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
		iflag = 1;
	    } else {
		visarr[newpos] = iv2gain[itemp - 1];
	    }
	    ++newpos;
	    ++codpos;
/* L30: */
	}
	r__1 = frmtgvarkb2_(&codarr[codpos]);
	if (pctdifgvarkb2_(&ivalb, &r__1) > mxdf && *hvcod == 1) {
	    visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
	    iflag = 1;
	} else {
	    visarr[newpos] = ivalb;
	}
    }
    if (*isat % 2 == 1) {
	for (itemp = 1; itemp <= 4; ++itemp) {
	    r__1 = frmtgvarkb2_(&codarr[codpos]);
	    if (pctdifgvarkb2_(&svbias[itemp - 1], &r__1) > mxdf && *hvcod == 
		    1) {
		visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
		iflag = 1;
	    } else {
		visarr[newpos] = svbias[itemp - 1];
	    }
	    ++newpos;
	    ++codpos;
/* L40: */
	}
	for (itemp = 1; itemp <= 4; ++itemp) {
	    r__1 = frmtgvarkb2_(&codarr[codpos]);
	    if (pctdifgvarkb2_(&svgain[itemp - 1], &r__1) > mxdf && *hvcod == 
		    1) {
		visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
		iflag = 1;
	    } else {
		visarr[newpos] = svgain[itemp - 1];
	    }
	    ++newpos;
	    ++codpos;
/* L50: */
	}
	for (itemp = 1; itemp <= 4; ++itemp) {
	    r__1 = frmtgvarkb2_(&codarr[codpos]);
	    if (pctdifgvarkb2_(&sv2gain[itemp - 1], &r__1) > mxdf && *hvcod ==
		     1) {
		visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
		iflag = 1;
	    } else {
		visarr[newpos] = sv2gain[itemp - 1];
	    }
	    ++newpos;
	    ++codpos;
/* L60: */
	}
	r__1 = frmtgvarkb2_(&codarr[codpos]);
	if (pctdifgvarkb2_(&svalb, &r__1) > mxdf && *hvcod == 1) {
	    visarr[newpos] = frmtgvarkb2_(&codarr[codpos]);
	    ++iflag;
	} else {
	    visarr[newpos] = svalb;
	}
    }
    return 0;
} /* chkvisgvarkb2_ */

/* Subroutine */ int chkirgvarkb2_(isat, iband, codarr, gain, bias, hvcod)
integer *isat, *iband, *codarr;
real *gain, *bias;
integer *hvcod;
{
    /* Initialized data */

    static integer ibiasoff = 25;
    static integer igainoff = 33;
    static integer sbiasoff = 13;
    static integer sgainoff = 31;
    static real iirgain[4] = { (float)5.2285,(float)5.02714,(float)227.389,(
	    float)38.8383 };
    static real iirbias[4] = { (float)15.6854,(float)15.3332,(float)68.2167,(
	    float)29.1287 };
    static real sirgain[18] = { (float)528.977,(float)540.005,(float)485.624,(
	    float)394.575,(float)357.802,(float)334.175,(float)311.523,(float)
	    314.603,(float)434.352,(float)1126.22,(float)1899.56,(float)
	    2874.34,(float)9642.75,(float)14105.4,(float)26221.3,(float)
	    10720.6,(float)12136.1,(float)19358.1 };
    static real sirbias[18] = { (float)1745.63,(float)1566.01,(float)1311.19,(
	    float)887.794,(float)787.164,(float)417.719,(float)249.218,(float)
	    251.683,(float)716.68,(float)900.979,(float)1139.74,(float)
	    2155.76,(float)626.779,(float)916.85,(float)1704.39,(float)
	    428.824,(float)497.581,(float)348.446 };
    static real mxdf = (float).001;

    /* System generated locals */
    real r__1;

    /* Local variables */
    static integer iflag;
    extern doublereal pctdifgvarkb2_(), frmtgvarkb2_();
    static integer loc2, loc3, loc4, loc5;

    /* Parameter adjustments */
    --codarr;

    /* Function Body */
    iflag = 0;
    if (*isat % 2 == 0) {
	if (*isat < 78) {
	    loc2 = 3;
	    loc3 = 4;
	    loc4 = 1;
	    loc5 = 2;
	} else {
	    loc2 = 1;
	    loc3 = 2;
	    loc4 = 3;
	    loc5 = 4;
	}
	if (*iband == 2) {
	    *bias = iirbias[2];
	    *gain = iirgain[2];
	    r__1 = frmtgvarkb2_(&codarr[ibiasoff + loc2]);
	    if (pctdifgvarkb2_(bias, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*bias = frmtgvarkb2_(&codarr[ibiasoff + loc2]);
	    }
	    r__1 = frmtgvarkb2_(&codarr[igainoff + loc2]);
	    if (pctdifgvarkb2_(gain, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*gain = frmtgvarkb2_(&codarr[igainoff + loc2]);
	    }
	}
	if (*iband == 3) {
	    *bias = iirbias[3];
	    *gain = iirgain[3];
	    r__1 = frmtgvarkb2_(&codarr[ibiasoff + loc3]);
	    if (pctdifgvarkb2_(bias, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*bias = frmtgvarkb2_(&codarr[ibiasoff + loc3]);
	    }
	    r__1 = frmtgvarkb2_(&codarr[igainoff + loc3]);
	    if (pctdifgvarkb2_(gain, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*gain = frmtgvarkb2_(&codarr[igainoff + loc3]);
	    }
	}
	if (*iband == 4) {
	    *bias = iirbias[0];
	    *gain = iirgain[0];
	    r__1 = frmtgvarkb2_(&codarr[ibiasoff + loc4]);
	    if (pctdifgvarkb2_(bias, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*bias = frmtgvarkb2_(&codarr[ibiasoff + loc4]);
	    }
	    r__1 = frmtgvarkb2_(&codarr[igainoff + loc4]);
	    if (pctdifgvarkb2_(gain, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*gain = frmtgvarkb2_(&codarr[igainoff + loc4]);
	    }
	}
	if (*iband == 5) {
	    *bias = iirbias[1];
	    *gain = iirgain[1];
	    r__1 = frmtgvarkb2_(&codarr[ibiasoff + loc5]);
	    if (pctdifgvarkb2_(bias, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*bias = frmtgvarkb2_(&codarr[ibiasoff + loc5]);
	    }
	    r__1 = frmtgvarkb2_(&codarr[igainoff + loc5]);
	    if (pctdifgvarkb2_(gain, &r__1) > mxdf && *hvcod == 1) {
		iflag = 1;
		*gain = frmtgvarkb2_(&codarr[igainoff + loc5]);
	    }
	}
    }
    if (*isat % 2 == 1) {
	r__1 = frmtgvarkb2_(&codarr[sgainoff + *iband]);
	if (pctdifgvarkb2_(&sirgain[*iband - 1], &r__1) > mxdf && *hvcod == 1)
		 {
	    iflag = 1;
	    *gain = frmtgvarkb2_(&codarr[sgainoff + *iband]);
	} else {
	    *gain = sirgain[*iband - 1];
	}
	r__1 = frmtgvarkb2_(&codarr[sbiasoff + *iband]);
	if (pctdifgvarkb2_(&sirbias[*iband - 1], &r__1) > mxdf && *hvcod == 1)
		 {
	    iflag = 1;
	    *bias = frmtgvarkb2_(&codarr[sbiasoff + *iband]);
	} else {
	    *bias = sirbias[*iband - 1];
	}
    }
    return 0;
} /* chkirgvarkb2_ */

/* Subroutine */ int dmparrgvarkb2_(array, start, end)
real *array;
integer *start, *end;
{
    /* Parameter adjustments */
    --array;

    /* Function Body */
    return 0;
} /* dmparrgvarkb2_ */

doublereal pctdifgvarkb2_(value1, value2)
real *value1, *value2;
{
    /* System generated locals */
    real ret_val, r__1;

    if (*value1 != (float)0.) {
	ret_val = (r__1 = (*value1 - *value2) / *value1, dabs(r__1));
    } else {
	if (*value2 != (float)0.) {
	    ret_val = (r__1 = (*value1 - *value2) / *value2, dabs(r__1));
	} else {
	    ret_val = (float)0.;
	}
    }
    return ret_val;
} /* pctdifgvarkb2_ */

integer chkcodgvarkb2_(iarr)
integer *iarr;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer icount, numint;

    /* Parameter adjustments */
    --iarr;

    /* Function Body */
    numint = 0;
    for (icount = 1; icount <= 20; ++icount) {
	if (iarr[icount] < 65536 && iarr[icount] > 0) {
	    ++numint;
	}
/* L54: */
    }
    if (numint > 10) {
	ret_val = 0;
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* chkcodgvarkb2_ */

/* Subroutine */ int exptabgvarkb2_(intab, itab, switch__)
integer *intab, *itab, *switch__;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer lbeg, lend, j, ibrit, lstep;
    static real offset, fstval;
    static integer lstval, ind1, ind2;

    /* Parameter adjustments */
    --itab;
    --intab;

    /* Function Body */
    if (*switch__ == 0) {
	lbeg = 1;
	lend = 254;
	lstep = 1;
	offset = (float)-1.;
	fstval = (float)0.;
	lstval = 255;
    } else if (*switch__ == 1) {
	lbeg = 255;
	lend = 1;
	lstep = -1;
	offset = (float)0.;
	fstval = (float)255.;
	lstval = 0;
    }
    ind1 = 2;
    ind2 = intab[lbeg] - 1;
    itab[1] = 0;
    i__1 = ind2;
    for (j = ind1; j <= i__1; ++j) {
	itab[j] = fstval;
/* L25: */
    }
    ind1 = ind2 + 1;
    lbeg += lstep;
    i__1 = lend;
    i__2 = lstep;
    for (ibrit = lbeg; i__2 < 0 ? ibrit >= i__1 : ibrit <= i__1; ibrit += 
	    i__2) {
	ind2 = intab[ibrit] - 1;
	i__3 = ind2;
	for (j = ind1; j <= i__3; ++j) {
	    itab[j] = ibrit + offset;
/* L50: */
	}
	ind1 = ind2 + 1;
/* L100: */
    }
    for (j = ind1; j <= 32768; ++j) {
	itab[j] = lstval;
/* L150: */
    }
    return 0;
} /* exptabgvarkb2_ */

