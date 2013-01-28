/* nv1rect.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real xrow, xcol, zslat, zslon, zdlat, zdlon;
    integer itype, iwest;
} rctcomrectnv1_;

#define rctcomrectnv1_1 rctcomrectnv1_

/* Table of constant values */

static real c_b4 = 10.f;
static doublereal c_b8 = 10.;

integer nv1inirect_(integer *ifunc, integer *iparms)
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    double pow_ri(real *, integer *), pow_di(doublereal *, integer *);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer ipowdele, ipowdlin;
    static real r__;
    extern integer lit_(char *, ftnlen);
    static doublereal decc, drad;
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);
    extern /* Subroutine */ int llopt_(doublereal *, doublereal *, integer *, 
	    integer *);
    static integer ipowecc, ipowrad, ipowlat, ipowlon;

/* Degrees Per Line Power */
/* Degrees Per Element Power */
/* Degrees Radian power */
/* Eccentricity Power */
/* ,ULLON */
    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    if (*ifunc == 1) {
	if (iparms[1] != lit_("RECT", (ftnlen)4)) {
	    goto L900;
	}
	rctcomrectnv1_1.itype = 1;
	rctcomrectnv1_1.xrow = (real) iparms[2];
	ipowlat = iparms[12];
	if (ipowlat == 0) {
	    ipowlat = 4;
	}
/* default is 10000 (10^4) */
	rctcomrectnv1_1.zslat = iparms[3] / pow_ri(&c_b4, &ipowlat);
/* REAL Latitude */
	rctcomrectnv1_1.xcol = (real) iparms[4];
	ipowlon = iparms[13];
	if (ipowlon == 0) {
	    ipowlon = 4;
	}
	rctcomrectnv1_1.zslon = iparms[5] / pow_ri(&c_b4, &ipowlon);
/* REAL Longitude */
	ipowdlin = iparms[14];
	if (ipowdlin == 0) {
	    ipowdlin = 4;
	}
	rctcomrectnv1_1.zdlat = iparms[6] / pow_ri(&c_b4, &ipowdlin);
/* REAL Degrees_per_line_latitude */
	ipowdele = iparms[15];
	if (ipowdele == 0) {
	    ipowdele = 4;
	}
	rctcomrectnv1_1.zdlon = iparms[7] / pow_ri(&c_b4, &ipowdele);
/* REAL Degrees_per_line_longitude */
	ipowrad = iparms[16];
	if (ipowrad == 0) {
	    ipowrad = 3;
	}
	drad = iparms[8] / pow_di(&c_b8, &ipowrad);
/* REAL Radius of the planet in mete */
	r__ = drad;
	ipowecc = iparms[17];
	if (ipowecc == 0) {
	    ipowecc = 6;
	}
	decc = iparms[9] / pow_di(&c_b8, &ipowecc);
/* REAL Eccentricity */
	rctcomrectnv1_1.iwest = iparms[11];
/* West positive vs. West negative */
	if (rctcomrectnv1_1.iwest >= 0) {
	    rctcomrectnv1_1.iwest = 1;
	}
	llopt_(&drad, &decc, &rctcomrectnv1_1.iwest, &iparms[10]);
/* Initialze LLCART c */
	if (rctcomrectnv1_1.xcol == 1.f) {
/* special case of XCOL not located at ima */
	    rctcomrectnv1_1.zslon -= rctcomrectnv1_1.iwest * 180.f;
/* -- so assume it's the left edge(duh) */
	}
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    rctcomrectnv1_1.itype = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    rctcomrectnv1_1.itype = 2;
	}
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* nv1inirect_ */

integer nv1saerect_(real *xlin, real *xele, real *xdum, real *xlat, real *
	xlon, real *z__)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real ylat, ylon, xedif, xldif;
    extern /* Subroutine */ int llcart_(real *, real *, real *, real *, real *
	    );

/* ,ULLON */
    xldif = rctcomrectnv1_1.xrow - *xlin;
    if (rctcomrectnv1_1.xcol == 1.f) {
	xedif = rctcomrectnv1_1.iwest * (*xele - rctcomrectnv1_1.xcol);
	*xlon = rctcomrectnv1_1.zslon + rctcomrectnv1_1.iwest * 180 - xedif * 
		rctcomrectnv1_1.zdlon;
    } else {
	xedif = rctcomrectnv1_1.iwest * (rctcomrectnv1_1.xcol - *xele);
	*xlon = rctcomrectnv1_1.zslon + xedif * rctcomrectnv1_1.zdlon;
    }
    *xlat = rctcomrectnv1_1.zslat + xldif * rctcomrectnv1_1.zdlat;
    if (*xlat > 90.f || *xlat < -90.f) {
	goto L900;
    }
    if (*xlon > rctcomrectnv1_1.zslon + 180.f) {
	goto L900;
    }
    if (*xlon < rctcomrectnv1_1.zslon - 180.f) {
	goto L900;
    }
    if (*xlon < -180.f) {
	*xlon += 360.f;
    } else if (*xlon > 180.f) {
	*xlon += -360.f;
    }
    if (rctcomrectnv1_1.itype == 1) {
	ylat = *xlat;
	ylon = *xlon;
	llcart_(&ylat, &ylon, xlat, xlon, z__);
    }
    ret_val = 0;
    return ret_val;
L900:
    ret_val = -1;
    return ret_val;
} /* nv1saerect_ */

integer nv1easrect_(real *zlat, real *zlon, real *z__, real *xlin, real *xele,
	 real *xdum)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real x, y, xlat, xlon;
    extern /* Subroutine */ int cartll_(real *, real *, real *, real *, real *
	    );

/* ,ULLON */
    xlat = *zlat;
    xlon = *zlon;
    if (rctcomrectnv1_1.itype == 1) {
	x = xlat;
	y = xlon;
	cartll_(&x, &y, z__, &xlat, &xlon);
    }
    if (xlon > rctcomrectnv1_1.zslon + 180.f) {
	xlon += -360.f;
    } else if (xlon < rctcomrectnv1_1.zslon - 180.f) {
	xlon += 360.f;
    }
    *xlin = rctcomrectnv1_1.xrow - (xlat - rctcomrectnv1_1.zslat) / 
	    rctcomrectnv1_1.zdlat;
    if (rctcomrectnv1_1.xcol == 1.f) {
	*xele = rctcomrectnv1_1.xcol - (xlon - rctcomrectnv1_1.zslon - 
		rctcomrectnv1_1.iwest * 180.f) / (rctcomrectnv1_1.zdlon * 
		rctcomrectnv1_1.iwest);
    } else {
	*xele = rctcomrectnv1_1.xcol - (xlon - rctcomrectnv1_1.zslon) / (
		rctcomrectnv1_1.zdlon * rctcomrectnv1_1.iwest);
    }
    ret_val = 0;
    return ret_val;
} /* nv1easrect_ */

integer nv1optrect_(integer *ifunc, real *xin, real *xout)
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);
    static char cfunc[4];
    extern /* Subroutine */ int llobl_(real *, real *);

    /* Parameter adjustments */
    --xout;
    --xin;

    /* Function Body */
    clit_(ch__1, (ftnlen)4, ifunc);
    s_copy(cfunc, ch__1, (ftnlen)4, (ftnlen)4);
    ret_val = 0;
    if (s_cmp(cfunc, "SPOS", (ftnlen)4, (ftnlen)4) == 0) {
	xout[1] = rctcomrectnv1_1.zslat;
	xout[2] = rctcomrectnv1_1.zslon;
    } else if (s_cmp(cfunc, "ORAD", (ftnlen)4, (ftnlen)4) == 0) {
	llobl_(&xin[1], &xout[1]);
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* nv1optrect_ */

