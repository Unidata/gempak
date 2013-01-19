/* nv2msat.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer ic;
    real h__, re, a, rp, pi, cdr, crd;
    integer lpsi2;
    real deltax, deltay, rflon;
} metxxxmsatnv2_;

#define metxxxmsatnv2_1 metxxxmsatnv2_

struct {
    integer ioff[3];
    real sublon;
} polyxxmsatnv2_;

#define polyxxmsatnv2_1 polyxxmsatnv2_

/* Table of constant values */

static integer c__3 = 3;

integer nv2inimsat_(integer *ifunc, integer *iparms)
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer lit_(char *, ftnlen);
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);
    extern /* Subroutine */ int movw_(integer *, integer *, integer *);
    extern real flalo_(integer *);

    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    if (*ifunc == 1) {
	if (iparms[1] != lit_("MSAT", (ftnlen)4)) {
	    ret_val = -1;
	    return ret_val;
	}
	movw_(&c__3, &iparms[4], polyxxmsatnv2_1.ioff);
	metxxxmsatnv2_1.h__ = 35785.845000000001f;
	metxxxmsatnv2_1.re = 6378.155f;
	metxxxmsatnv2_1.a = .0033670033670033669f;
	metxxxmsatnv2_1.rp = metxxxmsatnv2_1.re / (metxxxmsatnv2_1.a + 1.f);
	metxxxmsatnv2_1.pi = 3.141592653f;
	metxxxmsatnv2_1.cdr = metxxxmsatnv2_1.pi / 180.f;
	metxxxmsatnv2_1.crd = 180.f / metxxxmsatnv2_1.pi;
	metxxxmsatnv2_1.lpsi2 = 1;
	metxxxmsatnv2_1.deltax = .0071999999999999998f;
	metxxxmsatnv2_1.deltay = .0071999999999999998f;
	metxxxmsatnv2_1.rflon = 0.f;
	polyxxmsatnv2_1.sublon = flalo_(&iparms[7]);
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    metxxxmsatnv2_1.ic = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    metxxxmsatnv2_1.ic = 2;
	}
    }
    ret_val = 0;
    return ret_val;
} /* nv2inimsat_ */

integer nv2saemsat_(real *xlin, real *xele, real *xdum, real *xfi, real *xla, 
	real *z__)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double tan(doublereal), sqrt(doublereal), cos(doublereal), sin(doublereal)
	    , asin(doublereal), atan(doublereal);

    /* Local variables */
    static real x, y, yk, rs, xr, yr, xt, yt, zt, vmu, val1, val2, teta, ylat,
	     tanx, tany, ylon, xele2, xlin2, cosrf, sinrf;
    extern /* Subroutine */ int nllxyz_(real *, real *, real *, real *, real *
	    );

    xele2 = *xele / 2.f;
    xlin2 = *xlin / 2.f;
    x = 1250.5f - xele2;
    y = polyxxmsatnv2_1.ioff[2] - (xlin2 + polyxxmsatnv2_1.ioff[1] - 
	    polyxxmsatnv2_1.ioff[0]);
    xr = x;
    yr = y;
    x = xr * metxxxmsatnv2_1.lpsi2 * metxxxmsatnv2_1.deltax * 
	    metxxxmsatnv2_1.cdr;
    y = yr * metxxxmsatnv2_1.lpsi2 * metxxxmsatnv2_1.deltay * 
	    metxxxmsatnv2_1.cdr;
    rs = metxxxmsatnv2_1.re + metxxxmsatnv2_1.h__;
    tanx = tan(x);
    tany = tan(y);
    val1 = tanx * tanx + 1.f;
    val2 = tany * tany * ((metxxxmsatnv2_1.a + 1.f) * (metxxxmsatnv2_1.a + 
	    1.f)) + 1.f;
    yk = rs / metxxxmsatnv2_1.re;
    if (val1 * val2 > yk * yk / (yk * yk - 1)) {
	ret_val = -1;
	return ret_val;
    }
    vmu = (rs - metxxxmsatnv2_1.re * sqrt(yk * yk - (yk * yk - 1) * val1 * 
	    val2)) / (val1 * val2);
    cosrf = cos(metxxxmsatnv2_1.rflon * metxxxmsatnv2_1.cdr);
    sinrf = sin(metxxxmsatnv2_1.rflon * metxxxmsatnv2_1.cdr);
    xt = rs * cosrf + vmu * (tanx * sinrf - cosrf);
    yt = rs * sinrf - vmu * (tanx * cosrf + sinrf);
    zt = vmu * tany / cos(x);
    teta = asin(zt / metxxxmsatnv2_1.rp);
    *xfi = atan(tan(teta) * metxxxmsatnv2_1.re / metxxxmsatnv2_1.rp) * 
	    metxxxmsatnv2_1.crd;
    *xla = -atan(yt / xt) * metxxxmsatnv2_1.crd;
    *xla += polyxxmsatnv2_1.sublon;
    if (metxxxmsatnv2_1.ic == 1) {
	ylat = *xfi;
	ylon = *xla;
	nllxyz_(&ylat, &ylon, xfi, xla, z__);
    }
    ret_val = 0;
    return ret_val;
} /* nv2saemsat_ */

integer nv2easmsat_(real *vfi, real *vla, real *z__, real *yr, real *xr, real 
	*xdum)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal), tan(doublereal)
	    , atan(doublereal);

    /* Local variables */
    static real x, y, r1, r2, x1, y1, rs, px, py, xt, yt, zt, xla, xfi, rom, 
	    teta, reph, rpph, coslo, sinlo;
    extern /* Subroutine */ int nxyzll_(real *, real *, real *, real *, real *
	    );

    x1 = *vfi;
    y1 = -(*vla);
    if (metxxxmsatnv2_1.ic == 1) {
	x = *vfi;
	y = *vla;
	nxyzll_(&x, &y, z__, &x1, &y1);
	y1 = -y1;
    }
    y1 += polyxxmsatnv2_1.sublon;
    xfi = x1 * metxxxmsatnv2_1.cdr;
    xla = y1 * metxxxmsatnv2_1.cdr;
    rom = metxxxmsatnv2_1.re * metxxxmsatnv2_1.rp / sqrt(metxxxmsatnv2_1.rp * 
	    metxxxmsatnv2_1.rp * cos(xfi) * cos(xfi) + metxxxmsatnv2_1.re * 
	    metxxxmsatnv2_1.re * sin(xfi) * sin(xfi));
    y = sqrt(metxxxmsatnv2_1.h__ * metxxxmsatnv2_1.h__ + rom * rom - 
	    metxxxmsatnv2_1.h__ * 2 * rom * cos(xfi) * cos(xla));
    r1 = y * y + rom * rom;
    r2 = metxxxmsatnv2_1.h__ * metxxxmsatnv2_1.h__;
    if (r1 > r2) {
	ret_val = -1;
	return ret_val;
    }
    rs = metxxxmsatnv2_1.re + metxxxmsatnv2_1.h__;
    reph = metxxxmsatnv2_1.re;
    rpph = metxxxmsatnv2_1.rp;
    coslo = cos(metxxxmsatnv2_1.rflon * metxxxmsatnv2_1.cdr);
    sinlo = sin(metxxxmsatnv2_1.rflon * metxxxmsatnv2_1.cdr);
    teta = atan(rpph / reph * tan(xfi));
    xt = reph * cos(teta) * cos(xla);
    yt = reph * cos(teta) * sin(xla);
    zt = rpph * sin(teta);
    px = atan((coslo * (yt - rs * sinlo) - (xt - rs * coslo) * sinlo) / (
	    sinlo * (yt - rs * sinlo) + (xt - rs * coslo) * coslo));
    py = atan(zt * ((tan(px) * sinlo - coslo) / (xt - rs * coslo)) * cos(px));
    px *= metxxxmsatnv2_1.crd;
    py *= metxxxmsatnv2_1.crd;
    *xr = px / (metxxxmsatnv2_1.deltax * metxxxmsatnv2_1.lpsi2);
    *yr = py / (metxxxmsatnv2_1.deltay * metxxxmsatnv2_1.lpsi2);
    *xr = 1250.5f - *xr;
    *yr = *yr + polyxxmsatnv2_1.ioff[2] + polyxxmsatnv2_1.ioff[1] - 
	    polyxxmsatnv2_1.ioff[0];
    *xr *= 2;
    *yr = 5000 - *yr * 2;
    ret_val = 0;
    return ret_val;
} /* nv2easmsat_ */

integer nv2optmsat_(integer *ifunc, real *xin, real *xout)
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

    /* Parameter adjustments */
    --xout;
    --xin;

    /* Function Body */
    clit_(ch__1, (ftnlen)4, ifunc);
    s_copy(cfunc, ch__1, (ftnlen)4, (ftnlen)4);
    ret_val = 0;
    if (s_cmp(cfunc, "SPOS", (ftnlen)4, (ftnlen)4) == 0) {
	xout[1] = 0.f;
	xout[2] = polyxxmsatnv2_1.sublon;
    } else if (s_cmp(cfunc, "HGT ", (ftnlen)4, (ftnlen)4) == 0) {
	metxxxmsatnv2_1.re = xin[1] + 6378.155f;
	metxxxmsatnv2_1.a = .0033670033670033669f;
	metxxxmsatnv2_1.rp = metxxxmsatnv2_1.re / (metxxxmsatnv2_1.a + 1.f);
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* nv2optmsat_ */

