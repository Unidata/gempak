/* kb2aaa.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer itype, jtype, jopt[5];
} aaaxxaaakb2_;

#define aaaxxaaakb2_1 aaaxxaaakb2_

union {
    struct {
	integer iarr[128];
    } _1;
    struct {
	integer isss, iday, itime, iab[76]	/* was [2][38] */, ifab[38], 
		idum[11];
    } _2;
} calbxxaaakb2_;

#define calbxxaaakb2_1 (calbxxaaakb2_._1)
#define calbxxaaakb2_2 (calbxxaaakb2_._2)

struct {
    char caltyp[4];
} brkpntaaakb2_;

#define brkpntaaakb2_1 brkpntaaakb2_

/* Table of constant values */

static integer c__5 = 5;
static integer c__512 = 512;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__2 = 2;
static real c_b57 = (float)2.;

integer kb2iniaaa_(cin, cout, iopt, cin_len, cout_len)
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

    /* Parameter adjustments */
    --iopt;

    /* Function Body */
    movw_(&c__5, &iopt[1], aaaxxaaakb2_1.jopt);
    aaaxxaaakb2_1.itype = 0;
    if (s_cmp(cin, "RAW", (ftnlen)4, (ftnlen)3) == 0) {
	aaaxxaaakb2_1.itype = 1;
    }
    if (s_cmp(cin, "RAD", (ftnlen)4, (ftnlen)3) == 0) {
	aaaxxaaakb2_1.itype = 2;
    }
    if (s_cmp(cin, "TEMP", (ftnlen)4, (ftnlen)4) == 0) {
	aaaxxaaakb2_1.itype = 3;
    }
    if (s_cmp(cin, "BRIT", (ftnlen)4, (ftnlen)4) == 0) {
	aaaxxaaakb2_1.itype = 4;
    }
    if (aaaxxaaakb2_1.itype == 0) {
	goto L900;
    }
    aaaxxaaakb2_1.jtype = 0;
    if (s_cmp(cout, "RAD", (ftnlen)4, (ftnlen)3) == 0) {
	aaaxxaaakb2_1.jtype = 2;
	if (aaaxxaaakb2_1.jopt[1] < 4) {
	    goto L900;
	}
    }
    if (s_cmp(cout, "TEMP", (ftnlen)4, (ftnlen)4) == 0) {
	aaaxxaaakb2_1.jtype = 3;
	if (aaaxxaaakb2_1.jopt[1] < 2) {
	    goto L900;
	}
    }
    if (s_cmp(cout, "BRIT", (ftnlen)4, (ftnlen)4) == 0) {
	aaaxxaaakb2_1.jtype = 4;
    }
    if (s_cmp(cout, "MODB", (ftnlen)4, (ftnlen)4) == 0) {
	aaaxxaaakb2_1.jtype = 4;
    }
    if (aaaxxaaakb2_1.jtype == 0) {
	goto L900;
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* kb2iniaaa_ */

integer kb2calaaa_(pfx, idir, nval, iband, ibuf)
shortint *pfx;
integer *idir, *nval, *iband, *ibuf;
{
    /* Initialized data */

    static integer lstchl[2] = { -1,-1 };
    static integer lstara = -1;
    static integer lstvis = -1;
    static integer indchl = 2;

    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    extern /* Subroutine */ int visconaaakb2_();
    static integer ncal, ndoc, ncod, ides, nlev, isou, chanl, table[8192]	
	    /* was [4096][2] */;
    extern /* Subroutine */ int zeros_();
    static integer ia;
    extern /* Subroutine */ int maaatb_(), araget_();
    static integer nbands, calpos, vistab[4096];
    extern /* Subroutine */ int mpixtb_();
    static integer lstunt;
    extern /* Subroutine */ int irconaaakb2_();
    extern integer gtchanaaakb2_();

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

/* local variables */
/* initialized variables */
    /* Parameter adjustments */
    --ibuf;
    --idir;
    --pfx;

    /* Function Body */
    ia = idir[33];
    isou = aaaxxaaakb2_1.jopt[0];
    ides = aaaxxaaakb2_1.jopt[1];
    if (ia != lstara) {
	araget_(&ia, &idir[63], &c__512, calbxxaaakb2_1.iarr);
	calbxxaaakb2_1.iarr[0] = calbxxaaakb2_1.iarr[0] / 2 << 1;
	nbands = idir[14];
	ncod = 0;
	if (idir[36] != 0) {
	    ncod = 4;
	}
	ndoc = idir[49];
	ncal = idir[50];
	nlev = idir[51];
	calpos = ncod + ndoc + 12;
	lstara = ia;
    }
    if (*iband == 14) {
	if (lstvis == -1) {
	    visconaaakb2_(vistab);
	    lstvis = ia;
	}
	if (aaaxxaaakb2_1.jtype == 4) {
	    mpixtb_(nval, &isou, &ides, &ibuf[1], vistab);
	}
	goto L100;
    }
    chanl = gtchanaaakb2_(iband, &pfx[1], &calpos);
    if (chanl <= 0) {
	i__1 = *nval * ides;
	zeros_(&ibuf[1], &i__1);
	ret_val = -1;
	return ret_val;
    }
    switch ((int)aaaxxaaakb2_1.itype) {
	case 1:  goto L10;
	case 2:  goto L20;
	case 3:  goto L30;
	case 4:  goto L40;
    }
L10:
    switch ((int)aaaxxaaakb2_1.jtype) {
	case 1:  goto L100;
	case 2:  goto L11;
	case 3:  goto L12;
	case 4:  goto L13;
    }
L11:
    if (chanl != lstchl[indchl - 1] || aaaxxaaakb2_1.jtype != lstunt) {
	indchl = indchl % 2 + 1;
	if (chanl != lstchl[indchl - 1] || aaaxxaaakb2_1.jtype != lstunt) {
	    irconaaakb2_(&c__0, iband, &chanl, &table[(indchl << 12) - 4096]);
	}
	lstchl[indchl - 1] = chanl;
	lstunt = aaaxxaaakb2_1.jtype;
    }
    goto L50;
L12:
    if (chanl != lstchl[indchl - 1] || aaaxxaaakb2_1.jtype != lstunt) {
	indchl = indchl % 2 + 1;
	if (chanl != lstchl[indchl - 1] || aaaxxaaakb2_1.jtype != lstunt) {
	    irconaaakb2_(&c__1, iband, &chanl, &table[(indchl << 12) - 4096]);
	}
	lstchl[indchl - 1] = chanl;
	lstunt = aaaxxaaakb2_1.jtype;
    }
    goto L50;
L13:
    if (chanl != lstchl[indchl - 1] || aaaxxaaakb2_1.jtype != lstunt) {
	indchl = indchl % 2 + 1;
	if (chanl != lstchl[indchl - 1] || aaaxxaaakb2_1.jtype != lstunt) {
	    irconaaakb2_(&c__2, iband, &chanl, &table[(indchl << 12) - 4096]);
	}
	lstchl[indchl - 1] = chanl;
	lstunt = aaaxxaaakb2_1.jtype;
    }
    goto L50;
L20:
    switch ((int)aaaxxaaakb2_1.jtype) {
	case 1:  goto L100;
	case 2:  goto L100;
	case 3:  goto L100;
	case 4:  goto L100;
    }
L30:
    switch ((int)aaaxxaaakb2_1.jtype) {
	case 1:  goto L100;
	case 2:  goto L100;
	case 3:  goto L100;
	case 4:  goto L100;
    }
L40:
    switch ((int)aaaxxaaakb2_1.jtype) {
	case 1:  goto L100;
	case 2:  goto L100;
	case 3:  goto L100;
	case 4:  goto L100;
    }
L50:
    maaatb_(nval, &isou, &ides, &ibuf[1], &table[(indchl << 12) - 4096]);
L100:
    ret_val = 0;
    return ret_val;
} /* kb2calaaa_ */

integer kb2optaaa_(cfunc, iin, iout, cfunc_len)
char *cfunc;
integer *iin, *iout;
ftnlen cfunc_len;
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp();

    /* Local variables */
    static char cfile[8];
    extern /* Subroutine */ int movwc_();
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
	if (iin[4] == 14) {
	    iout[1] = 2;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("BRIT", (ftnlen)4);
	} else {
	    iout[1] = 4;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("RAD ", (ftnlen)4);
	    iout[4] = lit_("TEMP", (ftnlen)4);
	    iout[5] = lit_("BRIT", (ftnlen)4);
	}
	if (ischar_(&iin[38]) == 1) {
	    movwc_(&iin[38], cfile, (ftnlen)8);
	    if (brkset_(cfile, brkpntaaakb2_1.caltyp, (ftnlen)8, (ftnlen)4) !=
		     0) {
		ret_val = -3;
		return ret_val;
	    }
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "BRKP", (ftnlen)4, (ftnlen)4) == 0) {
	movwc_(&iin[1], cfile, (ftnlen)8);
	if (brkset_(cfile, brkpntaaakb2_1.caltyp, (ftnlen)8, (ftnlen)4) != 0) 
		{
	    ret_val = -3;
	    return ret_val;
	}
	ret_val = 0;
    } else if (s_cmp(cfunc, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	if (iin[1] < 1 || iin[1] > 14) {
	    ret_val = -2;
	    return ret_val;
	}
	if (iin[1] == 14) {
	    iout[1] = 2;
	    iout[2] = lit_("RAW ", (ftnlen)4);
	    iout[3] = lit_("BRIT", (ftnlen)4);
	    iout[4] = lit_("    ", (ftnlen)4);
	    iout[5] = lit_("    ", (ftnlen)4);
	    iout[6] = 1;
	    iout[7] = 1;
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
	    iout[11] = 1000;
	    iout[12] = 10;
	    iout[13] = 1;
	}
	ret_val = 0;
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* kb2optaaa_ */

integer gtchanaaakb2_(iband, pfx, calpos)
integer *iband;
shortint *pfx;
integer *calpos;
{
    /* Initialized data */

    static integer map[38] = { 1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,
	    10,11,12,3,4,5,7,8,9,10,3,4,5,7,8,9,10 };

    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    static integer ibeg, iend, ipos, i__, kband, chanl, istep;

/* local variables */
/* initialized variables */
    /* Parameter adjustments */
    --pfx;

    /* Function Body */
    ipos = *calpos / 2 + 1;
    kband = *iband;
    ibeg = 1;
    iend = 13;
    istep = 1;
    if (*iband == 13) {
	kband = 8;
	ibeg = 13;
	iend = 1;
	istep = -1;
    }
    i__1 = iend;
    i__2 = istep;
    for (i__ = ibeg; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	chanl = pfx[ipos];
	if (chanl < 1 || chanl > 38) {
	    goto L100;
	}
	if (map[chanl - 1] == kband) {
	    ret_val = chanl;
	    return ret_val;
	}
	ipos += 4;
L100:
	;
    }
    ret_val = -1;
    return ret_val;
} /* gtchanaaakb2_ */

/* Subroutine */ int visconaaakb2_(vistab)
integer *vistab;
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    integer s_cmp();
    double sqrt();
    integer i_nint();

    /* Local variables */
    static integer ival;
    static real xval;
    static integer i__;
    extern integer brkval_();

/* symbolic constants & shared data */
/* external functions */
    /* Parameter adjustments */
    --vistab;

    /* Function Body */
    if (s_cmp(brkpntaaakb2_1.caltyp, "RAW ", (ftnlen)4, (ftnlen)4) == 0) {
	for (i__ = 0; i__ <= 4095; ++i__) {
	    r__1 = (real) i__;
	    vistab[i__ + 1] = brkval_(&r__1);
/* L5: */
	}
    } else if (s_cmp(brkpntaaakb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 
	    0) {
	for (i__ = 0; i__ <= 4095; ++i__) {
	    xval = sqrt((real) i__) * (float)4.;
	    vistab[i__ + 1] = brkval_(&xval);
/* L6: */
	}
    } else {
	for (i__ = 0; i__ <= 4095; ++i__) {
	    r__1 = sqrt((real) i__) * (float)4.;
	    ival = i_nint(&r__1);
	    vistab[i__ + 1] = min(ival,255);
/* L7: */
	}
    }
    return 0;
} /* visconaaakb2_ */

/* Subroutine */ int irconaaakb2_(type__, band, chanl, table)
integer *type__, *band, *chanl, *table;
{
    /* System generated locals */
    integer i__1;
    real r__1, r__2;

    /* Builtin functions */
    double pow_ri();
    integer s_cmp(), i_nint();

    /* Local variables */
    extern integer grysclaaakb2_();
    static real xfab, temp[4096];
    static integer i__;
    static real xbrit, radval;
    extern integer brkval_();
    static real ab0, ab1;
    static integer fab;
    static real rad[4096];
    extern doublereal vastbbaaakb2_();

/* symbolic constants & shared data */
/* external functions */
/* local variables */
    /* Parameter adjustments */
    --table;

    /* Function Body */
    ab0 = (real) calbxxaaakb2_2.iab[(*chanl << 1) - 2];
    ab1 = (real) calbxxaaakb2_2.iab[(*chanl << 1) - 1];
    fab = calbxxaaakb2_2.ifab[*chanl - 1];
    i__1 = 15 - fab;
    xfab = pow_ri(&c_b57, &i__1);
    if (s_cmp(brkpntaaakb2_1.caltyp, "RAW ", (ftnlen)4, (ftnlen)4) == 0 && *
	    type__ == 2) {
	for (i__ = 2; i__ <= 4096; ++i__) {
	    r__1 = (real) (i__ - 1 << 3);
	    table[i__] = brkval_(&r__1);
/* L50: */
	}
	table[1] = 0;
	return 0;
    }
    if (s_cmp(brkpntaaakb2_1.caltyp, "RAD ", (ftnlen)4, (ftnlen)4) == 0 && *
	    type__ == 2) {
	for (i__ = 2; i__ <= 4096; ++i__) {
/* Computing MAX */
	    r__1 = (float)0., r__2 = (ab1 * (i__ - 1) / (float)4. - ab0) / 
		    xfab;
	    radval = dmax(r__1,r__2);
	    table[i__] = brkval_(&radval);
/* L100: */
	}
	table[1] = 0;
	return 0;
    } else {
	for (i__ = 2; i__ <= 4096; ++i__) {
/* Computing MAX */
	    r__1 = (float)0., r__2 = (ab1 * (i__ - 1) / (float)4. - ab0) / 
		    xfab;
	    rad[i__ - 1] = dmax(r__1,r__2);
	    r__1 = rad[i__ - 1] * (float)1e3;
	    table[i__] = i_nint(&r__1);
/* L110: */
	}
	table[1] = 0;
    }
    if (*type__ == 0) {
	return 0;
    }
    if (s_cmp(brkpntaaakb2_1.caltyp, "TEMP", (ftnlen)4, (ftnlen)4) == 0 && *
	    type__ == 2) {
	for (i__ = 2; i__ <= 4096; ++i__) {
	    r__1 = vastbbaaakb2_(&rad[i__ - 1], &calbxxaaakb2_2.isss, band);
	    table[i__] = brkval_(&r__1);
/* L200: */
	}
	table[1] = 0;
	return 0;
    } else {
	for (i__ = 2; i__ <= 4096; ++i__) {
	    temp[i__ - 1] = vastbbaaakb2_(&rad[i__ - 1], &calbxxaaakb2_2.isss,
		     band);
	    r__1 = temp[i__ - 1] * (float)10.;
	    table[i__] = i_nint(&r__1);
/* L210: */
	}
	table[1] = 0;
    }
    if (*type__ == 1) {
	return 0;
    }
    if (s_cmp(brkpntaaakb2_1.caltyp, "BRIT", (ftnlen)4, (ftnlen)4) == 0 && *
	    type__ == 2) {
	for (i__ = 2; i__ <= 4096; ++i__) {
	    xbrit = (real) grysclaaakb2_(&temp[i__ - 1]);
	    table[i__] = brkval_(&xbrit);
/* L300: */
	}
	table[1] = 0;
	return 0;
    } else {
	for (i__ = 2; i__ <= 4096; ++i__) {
	    table[i__] = grysclaaakb2_(&temp[i__ - 1]);
/* L310: */
	}
	table[1] = 0;
    }
    return 0;
} /* irconaaakb2_ */

doublereal vastbbaaakb2_(rad, isss, kch)
real *rad;
integer *isss, *kch;
{
    /* Initialized data */

    static real g6fk1[12] = { (float)3716.,(float)3936.,(float)4089.,(float)
	    4336.,(float)5019.,(float)1.28e5,(float)5855.,(float)8438.,(float)
	    31030.,(float)38790.,(float)136100.,(float)1.95e5 };
    static real g6fk2[12] = { (float)975.8,(float)994.8,(float)1007.,(float)
	    1027.,(float)1079.,(float)3175.,(float)1136.,(float)1283.,(float)
	    1980.,(float)2133.,(float)3241.,(float)3653. };
    static real g6tc[24]	/* was [2][12] */ = { (float)-2.859e-4,(float)
	    1.,(float).002472,(float)1.,(float).002334,(float)1.,(float)
	    .003948,(float).9999,(float).004031,(float).9999,(float).06857,(
	    float).9998,(float).004256,(float).9999,(float).3263,(float).9973,
	    (float).06822,(float).9997,(float).736,(float).9973,(float).05787,
	    (float).9999,(float).4213,(float).9991 };
    static real g7fk1[12] = { (float)3767.,(float)3940.,(float)4167.,(float)
	    4364.,(float)5025.,(float)129100.,(float)5851.,(float)8525.,(
	    float)31070.,(float)39200.,(float)132500.,(float)193200. };
    static real g7fk2[12] = { (float)980.3,(float)995.1,(float)1014.,(float)
	    1030.,(float)1079.,(float)3185.,(float)1135.,(float)1287.,(float)
	    1981.,(float)2140.,(float)3212.,(float)3642. };
    static real g7tc[24]	/* was [2][12] */ = { (float)-.001267,(float)
	    1.,(float).00318,(float).9999,(float).002383,(float)1.,(float)
	    .002739,(float).9999,(float).00536,(float).9999,(float).07353,(
	    float).9998,(float).004447,(float).9999,(float).3408,(float).9973,
	    (float).06478,(float).9998,(float).6448,(float).9977,(float).0583,
	    (float).9999,(float).415,(float).9991 };

    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double log();

    /* Local variables */
    static integer kkch;
    static real expn, tt;

/* local variables */
/* initialized variables */
    kkch = *kch;
    if (kkch == 13) {
	kkch = 8;
    }
    ret_val = (float)0.;
    if (*rad == (float)0.) {
	return ret_val;
    }
    if (*isss == 30) {
	expn = g6fk1[kkch - 1] / *rad + (float)1.;
	tt = g6fk2[kkch - 1] / log(expn);
	ret_val = (tt - g6tc[(kkch << 1) - 2]) / g6tc[(kkch << 1) - 1];
    } else if (*isss == 32) {
	expn = g7fk1[kkch - 1] / *rad + (float)1.;
	tt = g7fk2[kkch - 1] / log(expn);
	ret_val = (tt - g7tc[(kkch << 1) - 2]) / g7tc[(kkch << 1) - 1];
    }
    return ret_val;
} /* vastbbaaakb2_ */

integer grysclaaakb2_(tempk)
real *tempk;
{
    /* Initialized data */

    static integer con1 = 418;
    static integer con2 = 660;
    static real tlim = (float)242.;

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
} /* grysclaaakb2_ */

