/* nv3goes.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer navday, lintot;
    real deglin;
    integer ieltot;
    real degele, spinra;
    integer ietimy, ietimh;
    real semima, oeccen, orbinc, perhel, asnode;
    integer nopcln;
    real declin, rascen, piclin, prerat, predir, pitch, yaw, roll, skew;
} navcomgoesnv3_;

#define navcomgoesnv3_1 navcomgoesnv3_

struct {
    integer iajust, ibtcon, negbet, iseang;
} betcomgoesnv3_;

#define betcomgoesnv3_1 betcomgoesnv3_

struct {
    real scan1, time1, scan2, time2;
} vascomgoesnv3_;

#define vascomgoesnv3_1 vascomgoesnv3_

union {
    struct {
	real emega, ab, asq, bsq, r__, rsq, rdpdg;
	integer numsen;
	real totlin, radlin, totele, radele, picele, cpitch, cyaw, croll, 
		pskew, rfact, roasin, tmpscl, b11, b12, b13, b21, b22, b23, 
		b31, b32, b33, gamma, gamdot, rotm11, rotm13, rotm21, rotm23, 
		rotm31, rotm33, pictim, xref;
    } _1;
    struct {
	real emega, ab, asq, bsq, rr, rsq, rdpdg;
	integer numsen;
	real totlin, radlin, totele, radele, picele, cpitch, cyaw, croll, 
		pskew, rfact, roasin, tmpscl, b11, b12, b13, b21, b22, b23, 
		b31, b32, b33, gamma, gamdot, rotm11, rotm13, rotm21, rotm23, 
		rotm31, rotm33, pictim, xref;
    } _2;
} navinigoesnv3_;

#define navinigoesnv3_1 (navinigoesnv3_._1)
#define navinigoesnv3_2 (navinigoesnv3_._2)

struct {
    integer llsw, iold;
} nvunitgoesnv3_;

#define nvunitgoesnv3_1 nvunitgoesnv3_

/* Table of constant values */

static integer c__0 = 0;
static real c_b10 = (float)0.;
static integer c__1 = 1;

integer nv3inigoes_(ifunc, iarr)
integer *ifunc, *iarr;
{
    /* Initialized data */

    static integer misval = -2139062144;
    static integer jinit = 0;

    /* System generated locals */
    integer ret_val;
    real r__1, r__2;

    /* Builtin functions */
    integer s_cmp();
    double atan2(), sin(), cos();

    /* Local variables */
    static integer jday;
    extern /* Subroutine */ int epochgoesnv3_();
    static integer n;
    /* changed this to real 
       extern doublereal flalo_(); */
    extern real flalo_();
    static integer jtime;
    static char cllsw[2];
    extern /* Subroutine */ int movwc_();
    static integer jtype;
    extern doublereal raerac_();
    static real cosdec, sindec, xmeana, cosras;
    static integer jdaysv;
    extern integer iround_();
    static real sinras;
    static integer jtimsv;
    static real dec, ctp;
    extern integer lit_();
    static real ctr, ras;
    static integer iss;
    static real cty, stp, str, sty;
    extern integer icon1goesnv3_();

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: hex80.inc,v 1.8 1997/10/10 20:16:12 dglo Exp $ *** */
    /* Parameter adjustments */
    --iarr;

    /* Function Body */
    if (jinit == 0) {
	jinit = 1;
	nvunitgoesnv3_1.llsw = 0;
	jdaysv = -1;
	jtimsv = -1;
	nvunitgoesnv3_1.iold = 0;
    }
    if (*ifunc == 2) {
	movwc_(&iarr[1], cllsw, (ftnlen)2);
	if (s_cmp(cllsw, "LL", (ftnlen)2, (ftnlen)2) == 0) {
	    nvunitgoesnv3_1.llsw = 0;
	}
	if (s_cmp(cllsw, "XY", (ftnlen)2, (ftnlen)2) == 0) {
	    nvunitgoesnv3_1.llsw = 1;
	}
	ret_val = 0;
	return ret_val;
    }
    jtype = iarr[1];
    if (jtype != lit_("GOES", (ftnlen)4)) {
	goto L90;
    }
    jday = iarr[2];
    jtime = iarr[3];
    if (jday == jdaysv && jtime == jtimsv) {
	goto L10;
    }
    navcomgoesnv3_1.navday = jday % 100000;
    for (n = 7; n <= 12; ++n) {
	if (iarr[n] > 0) {
	    goto L25;
	}
/* L20: */
    }
    goto L90;
L25:
    navcomgoesnv3_1.ietimy = icon1goesnv3_(&iarr[5]);
    r__1 = iarr[6] % 100 * (float).6;
    navcomgoesnv3_1.ietimh = iarr[6] / 100 * 100 + iround_(&r__1);
    navcomgoesnv3_1.semima = (real) iarr[7] / (float)100.;
    navcomgoesnv3_1.oeccen = (real) iarr[8] / (float)1e6;
    navcomgoesnv3_1.orbinc = (real) iarr[9] / (float)1e3;
    xmeana = (real) iarr[10] / (float)1e3;
    navcomgoesnv3_1.perhel = (real) iarr[11] / (float)1e3;
    navcomgoesnv3_1.asnode = (real) iarr[12] / (float)1e3;
    epochgoesnv3_(&navcomgoesnv3_1.ietimy, &navcomgoesnv3_1.ietimh, &
	    navcomgoesnv3_1.semima, &navcomgoesnv3_1.oeccen, &xmeana);
    if (iarr[5] == 0) {
	goto L90;
    }
    navcomgoesnv3_1.declin = flalo_(&iarr[13]);
    navcomgoesnv3_1.rascen = flalo_(&iarr[14]);
    navcomgoesnv3_1.piclin = (real) iarr[15];
    if (iarr[15] >= 1000000) {
	navcomgoesnv3_1.piclin /= (float)1e4;
    }
    if (iarr[13] == 0 && iarr[14] == 0 && iarr[15] == 0) {
	goto L90;
    }
    navcomgoesnv3_1.spinra = iarr[16] / (float)1e3;
    if (iarr[16] != 0 && navcomgoesnv3_1.spinra < (float)300.) {
	navcomgoesnv3_1.spinra = (float)6e4 / navcomgoesnv3_1.spinra;
    }
    if (iarr[16] == 0) {
	goto L90;
    }
    navcomgoesnv3_1.deglin = flalo_(&iarr[17]);
    navcomgoesnv3_1.lintot = iarr[18];
    navcomgoesnv3_1.degele = flalo_(&iarr[19]);
    navcomgoesnv3_1.ieltot = iarr[20];
    navcomgoesnv3_1.pitch = flalo_(&iarr[21]);
    navcomgoesnv3_1.yaw = flalo_(&iarr[22]);
    navcomgoesnv3_1.roll = flalo_(&iarr[23]);
    navcomgoesnv3_1.skew = iarr[29] / (float)1e5;
    if (iarr[29] == misval) {
	navcomgoesnv3_1.skew = (float)0.;
    }
    betcomgoesnv3_1.iajust = iarr[25];
    betcomgoesnv3_1.iseang = iarr[28];
    betcomgoesnv3_1.ibtcon = 6289920;
    betcomgoesnv3_1.negbet = 3144960;
    navinigoesnv3_1.emega = (float).26251617;
    navinigoesnv3_1.ab = (float)40546851.22;
    navinigoesnv3_1.asq = (float)40683833.48;
    navinigoesnv3_1.bsq = (float)40410330.18;
    navinigoesnv3_1.r__ = (float)6371.221;
    navinigoesnv3_1.rsq = navinigoesnv3_1.r__ * navinigoesnv3_1.r__;
    navinigoesnv3_1.rdpdg = (float).01745329252;
    navinigoesnv3_1.numsen = navcomgoesnv3_1.lintot / 100000 % 100;
    if (navinigoesnv3_1.numsen < 1) {
	navinigoesnv3_1.numsen = 1;
    }
    navinigoesnv3_1.totlin = (real) (navinigoesnv3_1.numsen * (
	    navcomgoesnv3_1.lintot % 100000));
    navinigoesnv3_1.radlin = navinigoesnv3_1.rdpdg * navcomgoesnv3_1.deglin / 
	    (navinigoesnv3_1.totlin - (float)1.);
    navinigoesnv3_1.totele = (real) navcomgoesnv3_1.ieltot;
    navinigoesnv3_1.radele = navinigoesnv3_1.rdpdg * navcomgoesnv3_1.degele / 
	    (navinigoesnv3_1.totele - (float)1.);
    navinigoesnv3_1.picele = (navinigoesnv3_1.totele + (float)1.) / (float)2.;
    navinigoesnv3_1.cpitch = navinigoesnv3_1.rdpdg * navcomgoesnv3_1.pitch;
    navinigoesnv3_1.cyaw = navinigoesnv3_1.rdpdg * navcomgoesnv3_1.yaw;
    navinigoesnv3_1.croll = navinigoesnv3_1.rdpdg * navcomgoesnv3_1.roll;
    navinigoesnv3_1.pskew = atan2(navcomgoesnv3_1.skew, 
	    navinigoesnv3_1.radlin / navinigoesnv3_1.radele);
    stp = sin(navinigoesnv3_1.cpitch);
    ctp = cos(navinigoesnv3_1.cpitch);
    sty = sin(navinigoesnv3_1.cyaw - navinigoesnv3_1.pskew);
    cty = cos(navinigoesnv3_1.cyaw - navinigoesnv3_1.pskew);
    str = sin(navinigoesnv3_1.croll);
    ctr = cos(navinigoesnv3_1.croll);
    navinigoesnv3_1.rotm11 = ctr * ctp;
    navinigoesnv3_1.rotm13 = sty * str * ctp + cty * stp;
    navinigoesnv3_1.rotm21 = -str;
    navinigoesnv3_1.rotm23 = sty * ctr;
    navinigoesnv3_1.rotm31 = -ctr * stp;
    navinigoesnv3_1.rotm33 = cty * ctp - sty * str * stp;
/* Computing 2nd power */
    r__1 = navinigoesnv3_1.rotm31;
/* Computing 2nd power */
    r__2 = navinigoesnv3_1.rotm33;
    navinigoesnv3_1.rfact = r__1 * r__1 + r__2 * r__2;
    navinigoesnv3_1.roasin = atan2(navinigoesnv3_1.rotm31, 
	    navinigoesnv3_1.rotm33);
    navinigoesnv3_1.tmpscl = navcomgoesnv3_1.spinra / (float)3.6e6;
    dec = navcomgoesnv3_1.declin * navinigoesnv3_1.rdpdg;
    sindec = sin(dec);
    cosdec = cos(dec);
    ras = navcomgoesnv3_1.rascen * navinigoesnv3_1.rdpdg;
    sinras = sin(ras);
    cosras = cos(ras);
    navinigoesnv3_1.b11 = -sinras;
    navinigoesnv3_1.b12 = cosras;
    navinigoesnv3_1.b13 = (float)0.;
    navinigoesnv3_1.b21 = -sindec * cosras;
    navinigoesnv3_1.b22 = -sindec * sinras;
    navinigoesnv3_1.b23 = cosdec;
    navinigoesnv3_1.b31 = cosdec * cosras;
    navinigoesnv3_1.b32 = cosdec * sinras;
    navinigoesnv3_1.b33 = sindec;
    navinigoesnv3_1.xref = raerac_(&navcomgoesnv3_1.navday, &c__0, &c_b10) * 
	    navinigoesnv3_1.rdpdg;
    navinigoesnv3_1.pictim = flalo_(&jtime);
    navinigoesnv3_1.gamma = (real) iarr[39] / (float)100.;
    navinigoesnv3_1.gamdot = (real) iarr[40] / (float)100.;
    iss = jday / 100000;
    if ((iss > 25 || iss == 12) && iarr[31] > 0) {
	vascomgoesnv3_1.scan1 = (real) iarr[31];
	vascomgoesnv3_1.time1 = flalo_(&iarr[32]);
	vascomgoesnv3_1.scan2 = (real) iarr[35];
	vascomgoesnv3_1.time2 = flalo_(&iarr[36]);
    } else {
	vascomgoesnv3_1.scan1 = (float)1.;
	vascomgoesnv3_1.time1 = flalo_(&jtime);
	vascomgoesnv3_1.scan2 = (real) (navcomgoesnv3_1.lintot % 100000);
	vascomgoesnv3_1.time2 = vascomgoesnv3_1.time1 + vascomgoesnv3_1.scan2 
		* navinigoesnv3_1.tmpscl;
    }
    nvunitgoesnv3_1.iold = 0;
L10:
    jdaysv = jday;
    jtimsv = jtime;
    ret_val = 0;
    return ret_val;
L90:
    ret_val = -1;
    return ret_val;
} /* nv3inigoes_ */

integer nv3saegoes_(xlin, xele, xdum, xpar, ypar, zpar)
real *xlin, *xele, *xdum, *xpar, *ypar, *zpar;
{
    /* Initialized data */

    static real pi = (float)3.14159265;

    /* System generated locals */
    integer ret_val;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double atan2(), cos(), sin(), sqrt();

    /* Local variables */
    static real basq;
    static integer ilin;
    static real yele, temp, xsat, ysat, zsat, ylin, xcor, ycor, s, x, y, z__;
    extern /* Subroutine */ int satvecgoesnv3_();
    static real x1, y1;
    extern /* Subroutine */ int nxyzllgoesnv3_();
    static real aq, bq, cq, ct, st, framet, parlin, coslin, samtim, sinlin;
    extern integer iround_();
    static real sinele, cosele, onemsq, rad, eli, emi, eni, elo, emo, eno, 
	    rot;

    ilin = iround_(xlin);
    parlin = (real) ((ilin - 1) / navinigoesnv3_1.numsen + 1);
    framet = navinigoesnv3_1.tmpscl * parlin;
    samtim = framet + navinigoesnv3_1.pictim;
    satvecgoesnv3_(&samtim, &xsat, &ysat, &zsat);
    ylin = (*xlin - navcomgoesnv3_1.piclin) * navinigoesnv3_1.radlin;
    yele = (*xele - navinigoesnv3_1.picele + navinigoesnv3_1.gamma + 
	    navinigoesnv3_1.gamdot * samtim) * navinigoesnv3_1.radele;
    xcor = navinigoesnv3_1.b11 * xsat + navinigoesnv3_1.b12 * ysat + 
	    navinigoesnv3_1.b13 * zsat;
    ycor = navinigoesnv3_1.b21 * xsat + navinigoesnv3_1.b22 * ysat + 
	    navinigoesnv3_1.b23 * zsat;
    rot = atan2(ycor, xcor) + pi;
    yele -= rot;
    coslin = cos(ylin);
    sinlin = sin(ylin);
    sinele = sin(yele);
    cosele = cos(yele);
    eli = navinigoesnv3_1.rotm11 * coslin - navinigoesnv3_1.rotm13 * sinlin;
    emi = navinigoesnv3_1.rotm21 * coslin - navinigoesnv3_1.rotm23 * sinlin;
    eni = navinigoesnv3_1.rotm31 * coslin - navinigoesnv3_1.rotm33 * sinlin;
    temp = eli;
    eli = cosele * eli + sinele * emi;
    emi = -sinele * temp + cosele * emi;
    elo = navinigoesnv3_1.b11 * eli + navinigoesnv3_1.b21 * emi + 
	    navinigoesnv3_1.b31 * eni;
    emo = navinigoesnv3_1.b12 * eli + navinigoesnv3_1.b22 * emi + 
	    navinigoesnv3_1.b32 * eni;
    eno = navinigoesnv3_1.b13 * eli + navinigoesnv3_1.b23 * emi + 
	    navinigoesnv3_1.b33 * eni;
    basq = navinigoesnv3_1.bsq / navinigoesnv3_1.asq;
    onemsq = (float)1. - basq;
/* Computing 2nd power */
    r__1 = eno;
    aq = basq + onemsq * (r__1 * r__1);
    bq = ((elo * xsat + emo * ysat) * basq + eno * zsat) * (float)2.;
/* Computing 2nd power */
    r__1 = xsat;
/* Computing 2nd power */
    r__2 = ysat;
/* Computing 2nd power */
    r__3 = zsat;
    cq = (r__1 * r__1 + r__2 * r__2) * basq + r__3 * r__3 - 
	    navinigoesnv3_1.bsq;
/* Computing 2nd power */
    r__1 = bq;
    rad = r__1 * r__1 - aq * (float)4. * cq;
    if (rad < (float)1.) {
	goto L2;
    }
    s = -(bq + sqrt(rad)) / (aq * (float)2.);
    x = xsat + elo * s;
    y = ysat + emo * s;
    z__ = zsat + eno * s;
    ct = cos(navinigoesnv3_1.emega * samtim + navinigoesnv3_1.xref);
    st = sin(navinigoesnv3_1.emega * samtim + navinigoesnv3_1.xref);
    x1 = ct * x + st * y;
    y1 = -st * x + ct * y;
    if (nvunitgoesnv3_1.llsw == 0) {
	nxyzllgoesnv3_(&x1, &y1, &z__, xpar, ypar);
	*zpar = (float)0.;
    } else {
	*xpar = x1;
	*ypar = y1;
	*zpar = z__;
    }
    ret_val = 0;
    return ret_val;
L2:
    ret_val = -1;
    return ret_val;
} /* nv3saegoes_ */

integer nv3easgoes_(xpar, ypar, zpar, xlin, xele, xdum)
real *xpar, *ypar, *zpar, *xlin, *xele, *xdum;
{
    /* Initialized data */

    static real oldlin = (float)910.;
    static real orbtim = (float)-99999.;

    /* System generated locals */
    integer ret_val;
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), cos(), sin(), atan2(), asin();

    /* Local variables */
    static real cosa, clin, slin, xsat, ysat, zsat, ctst;
    static integer i__;
    static real u, v, x, y, z__;
    extern /* Subroutine */ int satvecgoesnv3_();
    static real xnorm, ynorm, znorm, x1, y1, x3;
    extern /* Subroutine */ int nllxyzgoesnv3_();
    static real vcste1, vcste2, vcste3, vcses3, vcses1, xsats1, vcses2, 
	    ysats2, ct, st, parlin, scnfrc, samtim, scnnum, xht, umv;

    ret_val = 0;
    if (nvunitgoesnv3_1.llsw == 0) {
	if (dabs(*xpar) > (float)90.) {
	    ret_val = -1;
	    return ret_val;
	}
	nllxyzgoesnv3_(xpar, ypar, &x1, &y1, &z__);
    } else {
	x1 = *xpar;
	y1 = *ypar;
	z__ = *zpar;
    }
    *xdum = (float)0.;
    samtim = vascomgoesnv3_1.time1;
    for (i__ = 1; i__ <= 2; ++i__) {
	if ((r__1 = samtim - orbtim, dabs(r__1)) < (float)5e-4) {
	    goto L10;
	}
	satvecgoesnv3_(&samtim, &xsat, &ysat, &zsat);
	orbtim = samtim;
/* Computing 2nd power */
	r__1 = xsat;
/* Computing 2nd power */
	r__2 = ysat;
/* Computing 2nd power */
	r__3 = zsat;
	xht = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
L10:
	ct = cos(navinigoesnv3_1.emega * samtim + navinigoesnv3_1.xref);
	st = sin(navinigoesnv3_1.emega * samtim + navinigoesnv3_1.xref);
	x = ct * x1 - st * y1;
	y = st * x1 + ct * y1;
	vcste1 = x - xsat;
	vcste2 = y - ysat;
	vcste3 = z__ - zsat;
	vcses3 = navinigoesnv3_1.b31 * vcste1 + navinigoesnv3_1.b32 * vcste2 
		+ navinigoesnv3_1.b33 * vcste3;
/* Computing 2nd power */
	r__1 = vcste1;
/* Computing 2nd power */
	r__2 = vcste2;
/* Computing 2nd power */
	r__3 = vcste3;
	znorm = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
	x3 = vcses3 / znorm;
/* Computing 2nd power */
	r__1 = x3;
	umv = atan2(x3, sqrt(navinigoesnv3_1.rfact - r__1 * r__1)) - 
		navinigoesnv3_1.roasin;
	*xlin = navcomgoesnv3_1.piclin - umv / navinigoesnv3_1.radlin;
	parlin = (real) ((integer) (*xlin - (float)1.) / 
		navinigoesnv3_1.numsen);
	if (i__ == 2) {
	    goto L50;
	}
	samtim = vascomgoesnv3_1.time2;
	oldlin = *xlin;
L50:
	;
    }
    scnnum = ((integer) (oldlin + *xlin) / (float)2. - (float)1.) / 
	    navinigoesnv3_1.numsen;
    scnfrc = (scnnum - vascomgoesnv3_1.scan1) / (vascomgoesnv3_1.scan2 - 
	    vascomgoesnv3_1.scan1);
    *xlin = oldlin + scnfrc * (*xlin - oldlin);
    samtim = vascomgoesnv3_1.time1 + navinigoesnv3_1.tmpscl * (scnnum - 
	    vascomgoesnv3_1.scan1);
    satvecgoesnv3_(&samtim, &xsat, &ysat, &zsat);
    cosa = x * xsat + y * ysat + z__ * zsat;
    ctst = navinigoesnv3_1.r__ * (float)1e-4 * xht + navinigoesnv3_1.rsq;
    if (cosa < ctst) {
	ret_val = -1;
    }
    xsats1 = navinigoesnv3_1.b11 * xsat + navinigoesnv3_1.b12 * ysat + 
	    navinigoesnv3_1.b13 * zsat;
    ysats2 = navinigoesnv3_1.b21 * xsat + navinigoesnv3_1.b22 * ysat + 
	    navinigoesnv3_1.b23 * zsat;
    ct = cos(navinigoesnv3_1.emega * samtim + navinigoesnv3_1.xref);
    st = sin(navinigoesnv3_1.emega * samtim + navinigoesnv3_1.xref);
    x = ct * x1 - st * y1;
    y = st * x1 + ct * y1;
    vcste1 = x - xsat;
    vcste2 = y - ysat;
    vcste3 = z__ - zsat;
    vcses1 = navinigoesnv3_1.b11 * vcste1 + navinigoesnv3_1.b12 * vcste2 + 
	    navinigoesnv3_1.b13 * vcste3;
    vcses2 = navinigoesnv3_1.b21 * vcste1 + navinigoesnv3_1.b22 * vcste2 + 
	    navinigoesnv3_1.b23 * vcste3;
    vcses3 = navinigoesnv3_1.b31 * vcste1 + navinigoesnv3_1.b32 * vcste2 + 
	    navinigoesnv3_1.b33 * vcste3;
/* Computing 2nd power */
    r__1 = znorm;
/* Computing 2nd power */
    r__2 = vcses3;
    xnorm = sqrt(r__1 * r__1 - r__2 * r__2);
/* Computing 2nd power */
    r__1 = xsats1;
/* Computing 2nd power */
    r__2 = ysats2;
    ynorm = sqrt(r__1 * r__1 + r__2 * r__2);
/* Computing 2nd power */
    r__1 = vcste1;
/* Computing 2nd power */
    r__2 = vcste2;
/* Computing 2nd power */
    r__3 = vcste3;
    znorm = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
    x3 = vcses3 / znorm;
/* Computing 2nd power */
    r__1 = x3;
    umv = atan2(x3, sqrt(navinigoesnv3_1.rfact - r__1 * r__1)) - 
	    navinigoesnv3_1.roasin;
    slin = sin(umv);
    clin = cos(umv);
    u = navinigoesnv3_1.rotm11 * clin + navinigoesnv3_1.rotm13 * slin;
    v = navinigoesnv3_1.rotm21 * clin + navinigoesnv3_1.rotm23 * slin;
    *xele = navinigoesnv3_1.picele + asin((xsats1 * vcses2 - ysats2 * vcses1) 
	    / (xnorm * ynorm)) / navinigoesnv3_1.radele;
    *xele += atan2(v, u) / navinigoesnv3_1.radele;
    *xele = *xele - navinigoesnv3_1.gamma - navinigoesnv3_1.gamdot * samtim;
    return ret_val;
} /* nv3easgoes_ */

integer nv3optgoes_(ifunc, xin, xout)
integer *ifunc;
real *xin, *xout;
{
    /* Initialized data */

    static integer lastim = -1;
    static real a = (float)6378.388;
    static real b = (float)6356.912;
    static real rr = (float)6371.221;
    static integer lasday = -1;

    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy();
    integer s_cmp();

    /* Local variables */
    static real flat;
    static integer jday;
    extern /* Character */ VOID clit_();
    static real flon, xlat, szen, xlon, x, y, z__;
    static char cfunc[4];
    static integer jtime, inorb, ntime;
    extern /* Subroutine */ int anglesgoesnv3_(), satposgoesnv3_(), 
	    nxyzllgoesnv3_();
    static real relang, zenloc;
    extern integer iround_();
    extern /* Subroutine */ int solarp_();
    extern integer m0itime_();
    static real dec, gha, hgt;

    /* Parameter adjustments */
    --xout;
    --xin;

    /* Function Body */
    clit_(ch__1, (ftnlen)4, ifunc);
    s_copy(cfunc, ch__1, (ftnlen)4, (ftnlen)4);
    ret_val = 0;
    if (s_cmp(cfunc, "SPOS", (ftnlen)4, (ftnlen)4) == 0) {
	inorb = 0;
	ntime = m0itime_(&navinigoesnv3_1.pictim);
	satposgoesnv3_(&inorb, &ntime, &x, &y, &z__);
	nxyzllgoesnv3_(&x, &y, &z__, &xout[1], &xout[2]);
    } else if (s_cmp(cfunc, "ANG ", (ftnlen)4, (ftnlen)4) == 0) {
	jday = iround_(&xin[1]);
	jtime = m0itime_(&xin[2]);
	flat = xin[3];
	flon = xin[4];
	if (jday != lasday || jtime != lastim) {
	    solarp_(&jday, &jtime, &gha, &dec, &xlat, &xlon);
	    lasday = jday;
	    lastim = jtime;
	}
	anglesgoesnv3_(&jday, &jtime, &flat, &flon, &gha, &dec, &zenloc, &
		szen, &relang);
	xout[1] = zenloc;
	xout[2] = szen;
	xout[3] = relang;
    } else if (s_cmp(cfunc, "HGT ", (ftnlen)4, (ftnlen)4) == 0) {
	hgt = xin[1];
	navinigoesnv3_1.asq = (a + hgt) * (a + hgt);
	navinigoesnv3_1.bsq = (b + hgt) * (b + hgt);
	navinigoesnv3_1.ab = (a + hgt) * (b + hgt);
	navinigoesnv3_1.r__ = rr + hgt;
	navinigoesnv3_1.rsq = navinigoesnv3_1.r__ * navinigoesnv3_1.r__;
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* nv3optgoes_ */

integer icon1goesnv3_(yymmdd)
integer *yymmdd;
{
    /* Initialized data */

    static integer num[12] = { 0,31,59,90,120,151,181,212,243,273,304,334 };

    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer year, month, julday, day;

    year = *yymmdd / 10000 % 100;
    month = *yymmdd / 100 % 100;
    day = *yymmdd % 100;
    if (month < 0 || month > 12) {
	month = 1;
    }
    julday = day + num[month - 1];
    if (year % 4 == 0 && month > 2) {
	++julday;
    }
    ret_val = year * 1000 + julday;
    return ret_val;
} /* icon1goesnv3_ */

/* Subroutine */ int epochgoesnv3_(ietimy, ietimh, semima, oeccen, xmeana)
integer *ietimy, *ietimh;
real *semima, *oeccen, *xmeana;
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sqrt(), sin();

    /* Local variables */
    static integer iday, jday;
    static real time, xmmc;
    static integer jtot;
    static real time1;
    extern doublereal flalo_();
    static integer jyear;
    static real xmanom;
    extern integer m0itime_();

/* Computing 3rd power */
    r__1 = sqrt((float)6378.388 / *semima);
    xmmc = r__1 * (r__1 * r__1) * (float).07436574;
    xmanom = *xmeana * (float).017453292500000002;
    time = (xmanom - *oeccen * sin(xmanom)) / (xmmc * (float)60.);
    time1 = flalo_(ietimh);
    time = time1 - time;
    iday = 0;
    if (time > (float)48.) {
	goto L8;
    }
    if (time > (float)24.) {
	goto L1;
    }
    if (time < (float)-24.) {
	goto L2;
    }
    if (time < (float)0.) {
	goto L3;
    }
    goto L4;
L8:
    time += (float)-48.;
    iday = 2;
    goto L4;
L1:
    time += (float)-24.;
    iday = 1;
    goto L4;
L2:
    time += (float)48.;
    iday = -2;
    goto L4;
L3:
    time += (float)24.;
    iday = -1;
L4:
    *ietimh = m0itime_(&time);
    if (iday == 0) {
	return 0;
    }
    jyear = *ietimy / 1000 % 100;
    jyear += 1000;
    jday = *ietimy % 1000;
    jday += iday;
    if (jday < 1) {
	goto L5;
    }
    jtot = 366 - (jyear % 4 + 3) / 4;
    if (jday > jtot) {
	goto L6;
    }
    goto L7;
L5:
    --jyear;
    jday = 366 - (jyear % 4 + 3) / 4 + jday;
    goto L7;
L6:
    ++jyear;
    jday -= jtot;
L7:
    jyear %= 100;
    *ietimy = jyear * 1000 + jday;
    return 0;
} /* epochgoesnv3_ */

/* Subroutine */ int satvecgoesnv3_(samtim, x, y, z__)
real *samtim, *x, *y, *z__;
{
    /* Initialized data */

    static integer navsav = 0;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sin(), cos(), sqrt();

    /* Local variables */
    static integer irad;
    static doublereal dnav;
    static real xmmc;
    static integer iray;
    static real a;
    static integer iefac, i__;
    static real o, p;
    static integer infac;
    static doublereal tdife;
    extern doublereal flalo_();
    static doublereal rdpdg;
    static integer inavd, irayd, inavy;
    static doublereal twopi, ecanm1;
    static real ca, srome2;
    static doublereal de;
    static real co, cp, sa;
    static integer irafac;
    static doublereal re, te, pi, ecanom;
    static real so, sp, px;
    static doublereal tdifra, gracon, diftim;
    static integer irahms;
    static real py, pz, qx, qy;
    static doublereal solsid, xmanom;
    static real qz, epsiln, timsam, xomega, yomega;
    static integer ied;
    static doublereal dra, sha, tra;
    static integer iey;
    static doublereal pi720;

    if (nvunitgoesnv3_1.iold == 1) {
	goto L10;
    }
    nvunitgoesnv3_1.iold = 1;
    navsav = navcomgoesnv3_1.navday;
    pi = 3.14159265;
    twopi = pi * (float)2.;
    pi720 = pi / (float)720.;
    rdpdg = pi / (float)180.;
    re = (float)6378.388;
    gracon = .07436574;
    solsid = 1.00273791;
    sha = 100.26467;
    sha = rdpdg * sha;
    irayd = 74001;
    irahms = 0;
    o = rdpdg * navcomgoesnv3_1.orbinc;
    p = rdpdg * navcomgoesnv3_1.perhel;
    a = rdpdg * navcomgoesnv3_1.asnode;
    so = sin(o);
    co = cos(o);
    sp = sin(p) * navcomgoesnv3_1.semima;
    cp = cos(p) * navcomgoesnv3_1.semima;
    sa = sin(a);
    ca = cos(a);
    px = cp * ca - sp * sa * co;
    py = cp * sa + sp * ca * co;
    pz = sp * so;
    qx = -sp * ca - cp * sa * co;
    qy = -sp * sa + cp * ca * co;
    qz = cp * so;
    srome2 = sqrt((float)1. - navcomgoesnv3_1.oeccen) * sqrt(
	    navcomgoesnv3_1.oeccen + (float)1.);
    xmmc = gracon * re * sqrt(re / navcomgoesnv3_1.semima) / 
	    navcomgoesnv3_1.semima;
    iey = navcomgoesnv3_1.ietimy / 1000 % 100;
    ied = navcomgoesnv3_1.ietimy % 1000;
    iefac = (iey - 1) / 4 + 1;
    de = (doublereal) ((iey - 1) * 365 + iefac + ied - 1);
    te = de * (float)1440. + flalo_(&navcomgoesnv3_1.ietimh) * (float)60.;
    iray = irayd / 1000;
    irad = irayd % 1000;
    irafac = (iray - 1) / 4 + 1;
    dra = (doublereal) ((iray - 1) * 365 + irafac + irad - 1);
    tra = dra * (float)1440. + flalo_(&irahms) * (float)60.;
    inavy = navcomgoesnv3_1.navday / 1000 % 100;
    inavd = navcomgoesnv3_1.navday % 1000;
    infac = (inavy - 1) / 4 + 1;
    dnav = (doublereal) ((inavy - 1) * 365 + infac + inavd - 1);
    tdife = dnav * (float)1440. - te;
    tdifra = dnav * (float)1440. - tra;
    epsiln = (float)1e-8;
L10:
    timsam = *samtim * (float)60.;
    diftim = tdife + timsam;
    xmanom = xmmc * diftim;
    ecanm1 = xmanom;
    for (i__ = 1; i__ <= 20; ++i__) {
	ecanom = xmanom + navcomgoesnv3_1.oeccen * sin(ecanm1);
	if ((d__1 = ecanom - ecanm1, abs(d__1)) < epsiln) {
	    goto L3;
	}
/* L2: */
	ecanm1 = ecanom;
    }
L3:
    xomega = cos(ecanom) - navcomgoesnv3_1.oeccen;
    yomega = srome2 * sin(ecanom);
    *z__ = xomega * pz + yomega * qz;
    *y = xomega * py + yomega * qy;
    *x = xomega * px + yomega * qx;
    return 0;
} /* satvecgoesnv3_ */

/* Subroutine */ int nllxyzgoesnv3_(xlat, xlon, x, y, z__)
real *xlat, *xlon, *x, *y, *z__;
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sin(), cos(), atan2(), sqrt();

    /* Local variables */
    static real csln, cslt, ylat, snln, ylon, snlt, tnlt, r__;

    ylat = navinigoesnv3_2.rdpdg * *xlat;
    ylat = atan2(navinigoesnv3_2.bsq * sin(ylat), navinigoesnv3_2.asq * cos(
	    ylat));
    ylon = -navinigoesnv3_2.rdpdg * *xlon;
    snlt = sin(ylat);
    cslt = cos(ylat);
    csln = cos(ylon);
    snln = sin(ylon);
/* Computing 2nd power */
    r__1 = snlt / cslt;
    tnlt = r__1 * r__1;
    r__ = navinigoesnv3_2.ab * sqrt((tnlt + (float)1.) / (navinigoesnv3_2.bsq 
	    + navinigoesnv3_2.asq * tnlt));
    *x = r__ * cslt * csln;
    *y = r__ * cslt * snln;
    *z__ = r__ * snlt;
    return 0;
} /* nllxyzgoesnv3_ */

/* Subroutine */ int nxyzllgoesnv3_(x, y, z__, xlat, xlon)
real *x, *y, *z__, *xlat, *xlon;
{
    /* Builtin functions */
    double sqrt(), atan(), sin(), cos(), atan2();

    /* Local variables */
    static real a;

    *xlat = (float)100.;
    *xlon = (float)200.;
    if (*x == (float)0. && *y == (float)0. && *z__ == (float)0.) {
	goto L90;
    }
    a = atan(*z__ / sqrt(*x * *x + *y * *y));
    *xlat = atan2(navinigoesnv3_1.asq * sin(a), navinigoesnv3_1.bsq * cos(a)) 
	    / navinigoesnv3_1.rdpdg;
    *xlon = -atan2(*y, *x) / navinigoesnv3_1.rdpdg;
L90:
    return 0;
} /* nxyzllgoesnv3_ */

/* Subroutine */ int anglesgoesnv3_(jday, jtime, xlat, xlon, gha, dec, satang,
	 sunang, relang)
integer *jday, *jtime;
real *xlat, *xlon, *gha, *dec, *satang, *sunang, *relang;
{
    /* Initialized data */

    static integer iday = 0;
    static real pi = (float)3.14159265;
    static real r__ = (float)6371.221;

    /* System generated locals */
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), sin(), cos(), acos(), atan2();

    /* Local variables */
    static real clat, sndc, clon, slat, snlg, xvec, yvec, zvec, ylat, xsam, 
	    slon, ysam, zsam, xsat, ysat, zsat, ylon, rdpdg;
    extern doublereal ftime_();
    static real xfact;
    static integer inorb;
    static real x1, y1, z1;
    extern /* Subroutine */ int satposgoesnv3_();
    static real x2, y2, x3, y3, z3, cosdec, height, us;
    extern doublereal geolat_();
    static real vs, ws, pictim, xc1, yc1, zc1, xc2, yc2, zc2, xan1, xan2, 
	    yan1, yan2, xan3, yan3;

    rdpdg = pi / (float)180.;
    if (iday == *jday) {
	goto L1;
    }
    iday = *jday;
    inorb = 0;
L1:
    pictim = ftime_(jtime);
    satposgoesnv3_(&inorb, jtime, &xsat, &ysat, &zsat);
/* Computing 2nd power */
    r__1 = xsat;
/* Computing 2nd power */
    r__2 = ysat;
/* Computing 2nd power */
    r__3 = zsat;
    height = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
    ylat = rdpdg * *xlat;
    ylat = geolat_(&ylat, &c__1);
    ylon = rdpdg * *xlon;
    slat = sin(ylat);
    clat = cos(ylat);
    slon = sin(ylon);
    clon = cos(ylon);
    xsam = r__ * clat * clon;
    ysam = r__ * clat * slon;
    zsam = r__ * slat;
    snlg = -pictim * pi / (float)12. - rdpdg * *gha;
    sndc = rdpdg * *dec;
    cosdec = cos(sndc);
    us = cos(snlg) * cosdec;
    vs = sin(snlg) * cosdec;
    ws = sin(sndc);
    *sunang = acos((us * xsam + vs * ysam + ws * zsam) / r__) / rdpdg;
    xvec = xsat - xsam;
    yvec = ysat - ysam;
    zvec = zsat - zsam;
/* Computing 2nd power */
    r__1 = xvec;
/* Computing 2nd power */
    r__2 = yvec;
/* Computing 2nd power */
    r__3 = zvec;
    xfact = sqrt(r__1 * r__1 + r__2 * r__2 + r__3 * r__3);
    *satang = acos((xvec * xsam + yvec * ysam + zvec * zsam) / (r__ * xfact)) 
	    / rdpdg;
    x1 = clat * clon;
    y1 = clat * slon;
    z1 = slat;
    x2 = slon;
    y2 = -clon;
    x3 = -slat * clon;
    y3 = -slat * slon;
    z3 = clat;
    xc1 = us - x1;
    yc1 = vs - y1;
    zc1 = ws - z1;
    xc2 = xsat / height - x1;
    yc2 = ysat / height - y1;
    zc2 = zsat / height - z1;
    xan1 = xc1 * x3 + yc1 * y3 + zc1 * z3;
    xan2 = xc2 * x3 + yc2 * y3 + zc2 * z3;
    yan1 = xc1 * x2 + yc1 * y2;
    yan2 = xc2 * x2 + yc2 * y2;
    xan3 = xan1 * xan2 + yan1 * yan2;
    yan3 = -yan1 * xan2 + xan1 * yan2;
    *relang = atan2(yan3, xan3) / rdpdg;
    *relang = dabs(*relang);
    return 0;
} /* anglesgoesnv3_ */

/* Subroutine */ int satposgoesnv3_(inorb, ntime, x, y, z__)
integer *inorb, *ntime;
real *x, *y, *z__;
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sin(), cos(), sqrt(), d_mod();

    /* Local variables */
    static doublereal xmmc, a;
    static integer i__;
    static doublereal o, p, rdpdg;
    static integer irayd;
    static doublereal ecanm1, ca, srome2, co, cp, sa, ra, re, pi, so, sp, 
	    ecanom, px, gracon, py;
    static integer irahms;
    static doublereal pz, qx, qy, qz, diftim;
    extern doublereal timdif_();
    static doublereal solsid, xmanom, epsiln, xomega, yomega, xs, ys, zs, cra,
	     sha, ras, sra;

    if (*inorb != 0) {
	goto L1;
    }
    *inorb = 1;
    pi = 3.14159265;
    rdpdg = pi / (float)180.;
    re = (float)6378.388;
    gracon = .07436574;
    solsid = 1.00273791;
    sha = 100.26467;
    sha = rdpdg * sha;
    irayd = 74001;
    irahms = 0;
    o = rdpdg * navcomgoesnv3_1.orbinc;
    p = rdpdg * navcomgoesnv3_1.perhel;
    a = rdpdg * navcomgoesnv3_1.asnode;
    so = sin(o);
    co = cos(o);
    sp = sin(p) * navcomgoesnv3_1.semima;
    cp = cos(p) * navcomgoesnv3_1.semima;
    sa = sin(a);
    ca = cos(a);
    px = cp * ca - sp * sa * co;
    py = cp * sa + sp * ca * co;
    pz = sp * so;
    qx = -sp * ca - cp * sa * co;
    qy = -sp * sa + cp * ca * co;
    qz = cp * so;
    srome2 = sqrt((float)1. - navcomgoesnv3_1.oeccen) * sqrt(
	    navcomgoesnv3_1.oeccen + (float)1.);
    xmmc = gracon * re * sqrt(re / navcomgoesnv3_1.semima) / 
	    navcomgoesnv3_1.semima;
L1:
    diftim = timdif_(&navcomgoesnv3_1.ietimy, &navcomgoesnv3_1.ietimh, &
	    navcomgoesnv3_1.navday, ntime);
    xmanom = xmmc * diftim;
    ecanm1 = xmanom;
    epsiln = (float)1e-8;
    for (i__ = 1; i__ <= 20; ++i__) {
	ecanom = xmanom + navcomgoesnv3_1.oeccen * sin(ecanm1);
	if ((d__1 = ecanom - ecanm1, abs(d__1)) < epsiln) {
	    goto L3;
	}
/* L2: */
	ecanm1 = ecanom;
    }
L3:
    xomega = cos(ecanom) - navcomgoesnv3_1.oeccen;
    yomega = srome2 * sin(ecanom);
    xs = xomega * px + yomega * qx;
    ys = xomega * py + yomega * qy;
    zs = xomega * pz + yomega * qz;
    diftim = timdif_(&irayd, &irahms, &navcomgoesnv3_1.navday, ntime);
    ra = diftim * solsid * pi / 720. + sha;
    d__1 = pi * (float)2.;
    ras = d_mod(&ra, &d__1);
    cra = cos(ras);
    sra = sin(ras);
    *x = cra * xs + sra * ys;
    *y = -sra * xs + cra * ys;
    *z__ = zs;
    return 0;
} /* satposgoesnv3_ */

