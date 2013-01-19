/* nv2ps.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real xrow, xcol, xlat1, xspace, xqlon, xblat;
    integer itype;
    real xpole, fac;
    integer ihem, iwest;
} pscompsnv2_;

#define pscompsnv2_1 pscompsnv2_

integer nv2inips_(integer *ifunc, integer *iparms)
{
    /* Initialized data */

    static real rad = .01745329f;

    /* System generated locals */
    integer ret_val, i__1;
    char ch__1[4];

    /* Builtin functions */
    double sin(doublereal), tan(doublereal);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real r__;
    extern integer lit_(char *, ftnlen);
    static doublereal decc, drad;
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);
    extern real flalo_(integer *);
    static integer ipole;
    extern /* Subroutine */ int llopt_(doublereal *, doublereal *, integer *, 
	    integer *);
    static real sclat1;

    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    if (*ifunc == 1) {
	if (iparms[1] != lit_("PS  ", (ftnlen)4)) {
	    ret_val = -1;
	    return ret_val;
	}
	pscompsnv2_1.itype = 1;
	pscompsnv2_1.xrow = (real) iparms[2];
	pscompsnv2_1.xcol = (real) iparms[3];
	ipole = iparms[11];
	if (ipole == 0) {
	    ipole = 900000;
	}
	pscompsnv2_1.ihem = 1;
	if (ipole < 0) {
	    pscompsnv2_1.ihem = -1;
	}
	pscompsnv2_1.xpole = flalo_(&ipole);
	i__1 = ipole - iparms[4];
	pscompsnv2_1.xlat1 = flalo_(&i__1) * rad;
	pscompsnv2_1.xspace = iparms[5] / 1e3f;
	pscompsnv2_1.xqlon = flalo_(&iparms[6]);
	drad = iparms[7] / 1e3;
	r__ = drad;
	decc = iparms[8] / 1e6;
	pscompsnv2_1.iwest = iparms[10];
	if (pscompsnv2_1.iwest >= 0) {
	    pscompsnv2_1.iwest = 1;
	}
	llopt_(&drad, &decc, &pscompsnv2_1.iwest, &iparms[9]);
	pscompsnv2_1.xblat = r__ * sin(pscompsnv2_1.xlat1) / (
		pscompsnv2_1.xspace * tan(pscompsnv2_1.xlat1 * .5f));
	sclat1 = (90.f - pscompsnv2_1.ihem * flalo_(&iparms[4])) * rad;
	pscompsnv2_1.fac = 1.f;
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    pscompsnv2_1.itype = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    pscompsnv2_1.itype = 2;
	}
    }
    ret_val = 0;
    return ret_val;
} /* nv2inips_ */

integer nv2saeps_(real *xlin, real *xele, real *xdum, real *xlat, real *xlon, 
	real *z__)
{
    /* Initialized data */

    static real rad = .01745329f;

    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double atan2(doublereal, doublereal), sqrt(doublereal), log(doublereal), 
	    exp(doublereal), atan(doublereal);

    /* Local variables */
    static real ylat, ylon, xedif, xldif, xrlon;
    extern /* Subroutine */ int llcart_(real *, real *, real *, real *, real *
	    );
    static real radius;

    xldif = pscompsnv2_1.ihem * (*xlin - pscompsnv2_1.xrow) / 
	    pscompsnv2_1.xblat;
    xedif = (pscompsnv2_1.xcol - *xele) / pscompsnv2_1.xblat;
    xrlon = 0.f;
    if (! (xldif == 0.f && xedif == 0.f)) {
	xrlon = atan2(xedif, xldif);
    }
    *xlon = pscompsnv2_1.iwest * xrlon / rad + pscompsnv2_1.xqlon;
    if (*xlon > 180.f) {
	*xlon += -360.f;
    }
    if (*xlon < -180.f) {
	*xlon += 360.f;
    }
    radius = sqrt(xldif * xldif + xedif * xedif);
    if (abs(radius) < 1e-10f) {
	*xlat = (real) (pscompsnv2_1.ihem * 90);
    } else {
	*xlat = pscompsnv2_1.ihem * (90.f - atan(exp(log(radius / 
		pscompsnv2_1.fac))) * 2 / rad);
    }
    if (pscompsnv2_1.itype == 1) {
	ylat = *xlat;
	ylon = *xlon;
	llcart_(&ylat, &ylon, xlat, xlon, z__);
    }
    ret_val = 0;
    return ret_val;
} /* nv2saeps_ */

integer nv2easps_(real *zlat, real *zlon, real *z__, real *xlin, real *xele, 
	real *xdum)
{
    /* Initialized data */

    static real rad = .01745329f;

    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double tan(doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    static real x, y, xlat, xlon, xclat, xrlat, xrlon;
    extern /* Subroutine */ int cartll_(real *, real *, real *, real *, real *
	    );

    xlat = *zlat;
    xlon = *zlon;
    if (pscompsnv2_1.itype == 1) {
	x = xlat;
	y = xlon;
	cartll_(&x, &y, z__, &xlat, &xlon);
    }
    xrlon = pscompsnv2_1.ihem * (xlon - pscompsnv2_1.xqlon);
    if (xrlon > 180.f) {
	xrlon += -360.f;
    }
    if (xrlon < -180.f) {
	xrlon += 360.f;
    }
    xrlon = pscompsnv2_1.iwest * xrlon * rad;
    xclat = (pscompsnv2_1.xpole - xlat) * rad * .5f;
    xrlat = pscompsnv2_1.xblat * tan(xclat);
    *xlin = xrlat * cos(xrlon) + pscompsnv2_1.xrow;
    *xele = -xrlat * sin(xrlon) + pscompsnv2_1.xcol;
    ret_val = 0;
    return ret_val;
} /* nv2easps_ */

integer nv2optps_(integer *ifunc, real *xin, real *xout)
{
    /* Initialized data */

    static real rad = .01745329f;

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
	xout[1] = pscompsnv2_1.xpole - pscompsnv2_1.xlat1 / rad;
	xout[2] = pscompsnv2_1.xqlon;
    } else if (s_cmp(cfunc, "ORAD", (ftnlen)4, (ftnlen)4) == 0) {
	llobl_(&xin[1], &xout[1]);
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* nv2optps_ */

