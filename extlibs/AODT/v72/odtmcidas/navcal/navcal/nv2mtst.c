/* nv2mtst.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer itype;
} mtstmtstnv2_;

#define mtstmtstnv2_1 mtstmtstnv2_

struct {
    integer loff, coff, lfac, cfac;
} nvparammtstnv2_;

#define nvparammtstnv2_1 nvparammtstnv2_

integer nv2inimtst_(integer *ifunc, integer *iparms)
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer lit_(char *, ftnlen);
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);

    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    mtstmtstnv2_1.itype = 0;
    ret_val = 0;
    if (*ifunc == 1) {
	if (iparms[1] != lit_("MTST", (ftnlen)4)) {
	    ret_val = -1;
	    return ret_val;
	}
	nvparammtstnv2_1.loff = iparms[2];
	nvparammtstnv2_1.coff = iparms[3];
	nvparammtstnv2_1.lfac = iparms[4];
	nvparammtstnv2_1.cfac = iparms[5];
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    mtstmtstnv2_1.itype = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    mtstmtstnv2_1.itype = 2;
	}
    }
    return ret_val;
} /* nv2inimtst_ */

integer nv2saemtst_(real *xlin, real *xele, real *xdum, real *xlat, real *
	xlon, real *z__)
{
    /* System generated locals */
    integer ret_val;
    real r__1, r__2;

    /* Local variables */
    static real a, b;
    extern /* Subroutine */ int mtst_to_llmtstnv2__(real *, real *, real *, 
	    real *), nllxyz_(real *, real *, real *, real *, real *);

    ret_val = 0;
    r__1 = 2750.f - (*xlin - 2) / 4.f;
    r__2 = 2750.f - (*xele - 2) / 4.f;
    mtst_to_llmtstnv2__(&r__1, &r__2, xlat, xlon);
    if (*xlat < -900.f) {
	ret_val = -1;
	return ret_val;
    }
    *xlon = -(*xlon);
    if (mtstmtstnv2_1.itype == 1) {
	a = *xlat;
	b = *xlon;
	nllxyz_(&a, &b, xlat, xlon, z__);
    }
    return ret_val;
} /* nv2saemtst_ */

integer nv2easmtst_(real *xlat, real *xlon, real *z__, real *xlin, real *xele,
	 real *xdum)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real a, b;
    extern /* Subroutine */ int ll_to_mtstmtstnv2__(real *, real *, real *, 
	    real *), nxyzll_(real *, real *, real *, real *, real *);

    ret_val = 0;
    a = *xlat;
    b = -(*xlon);
    if (mtstmtstnv2_1.itype == 1) {
	nxyzll_(xlat, xlon, z__, &a, &b);
	b = -b;
    }
    ll_to_mtstmtstnv2__(&a, &b, xlin, xele);
    if (*xlin < 0.f) {
	ret_val = -1;
	return ret_val;
    }
    *xlin = 11000 - ((*xlin - 2) * 4.f - 2.f);
    *xele = 11000 - ((*xele - 2) * 4.f - 2.f);
    *xlin += -1;
    *xele += -1;
    return ret_val;
} /* nv2easmtst_ */

integer nv2optmtst_(integer *ifunc, real *xin, real *xout)
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
    ret_val = 0;
    clit_(ch__1, (ftnlen)4, ifunc);
    s_copy(cfunc, ch__1, (ftnlen)4, (ftnlen)4);
    if (s_cmp(cfunc, "SPOS", (ftnlen)4, (ftnlen)4) == 0) {
	xout[1] = 0.f;
	xout[2] = -140.25f;
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* nv2optmtst_ */

/* Subroutine */ int ll_to_mtstmtstnv2__(real *xlat, real *xlon, real *xlin, 
	real *xele)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal), tan(doublereal)
	    , atan(doublereal);

    /* Local variables */
    static real a, h__, y, r1, r2, re, rp, rs, px, py, xr, yr, xt, yt, zt, 
	    cdr, crd, xla, xfi, rom, teta, deltax, deltay;


/*       S/R gives line and element for */
/*       a specified MTSAT latitude and longitude */

/*       Inputs: */
/*       xlat,xlon (REAL)  : latitude and longitude of selected point */
/*                           latitude North is positive, */
/*                           longitude East is positive */

/*       Outputs: */
/*       xlin, xele (REAL) : line and element number */
/*                           assumes that line 1 is in the South */
/*                           and element 1 is in the East */
/*                           based on 2750 lines and elements in total */
/*                           Output is -999. if specified xlat/xlon is */
/*                           not within MTSAT field-of-view */

/*       Subroutine assumes that line 1375 and element 1375 is 0/0 deg */

    re = 6378.155f;
    h__ = 42164.f - re;
    rs = re + h__;
    a = .0033670033670033669f;
    rp = re / (a + 1.f);
    cdr = .017453292500000002f;
    crd = 57.295779578552292f;
    deltax = 1.f / (nvparammtstnv2_1.cfac / 1e6f);
    deltay = 1.f / (nvparammtstnv2_1.lfac / 1e6f);
    xfi = *xlat * cdr;
    xla = (*xlon - 140.25f) * cdr;
    rom = re * rp / sqrt(rp * rp * cos(xfi) * cos(xfi) + re * re * sin(xfi) * 
	    sin(xfi));
    y = sqrt(h__ * h__ + rom * rom - h__ * 2.f * rom * cos(xfi) * cos(xla));
    r1 = y * y + rom * rom;
    r2 = h__ * h__;
    if (r1 > r2) {
	*xlin = -999.f;
	*xele = -999.f;
	return 0;
    }
    teta = atan(rp / re * tan(xfi));
    xt = re * cos(teta) * cos(xla);
    yt = re * cos(teta) * sin(xla);
    zt = rp * sin(teta);
    px = atan(yt / (xt - rs));
    py = atan(-zt / (xt - rs) * cos(px));
    px *= crd;
    py *= crd;
    xr = px / deltax;
    yr = py / deltay;
    *xele = nvparammtstnv2_1.coff / 10.f - xr;
    *xlin = nvparammtstnv2_1.loff / 10.f - yr;
    *xele = 2751.f - *xele;
    *xlin = 2751.f - *xlin;
    return 0;
} /* ll_to_mtstmtstnv2__ */

/* Subroutine */ int mtst_to_llmtstnv2__(real *xlin, real *xele, real *xlat, 
	real *xlon)
{
    /* Builtin functions */
    double tan(doublereal), sqrt(doublereal), cos(doublereal), asin(
	    doublereal), atan(doublereal);

    /* Local variables */
    static real a, h__, v1, v2, re, rp, yk, rs, xr, yr, xt, yt, zt, cdr, crd, 
	    vmu, teta, tanx, tany, deltax, deltay;


/*       S/R gives latitude and longitude for */
/*       a specified MTSAT line and element */

/*       Inputs: */
/*       xlin, xele (REAL) : line and element number */
/*                           assumes that line 1 is in the South */
/*                           and element 1 is in the East */
/*                           based on 2750 lines and elements in total */

/*       Outputs: */
/*       xlat,xlon (REAL)  : latitude and longitude of selected point */
/*                           latitude North is positive, */
/*                           longitude East is positive */
/*                           output is -999. if line/element is off the disk */

/*       Subroutine assumes that line 1375 and element 1375 is 0/0 deg */

    re = 6378.155f;
    h__ = 42164.f - re;
    rs = re + h__;
    yk = rs / re;
    a = .0033670033670033669f;
    rp = re / (a + 1.f);
    cdr = .017453292500000002f;
    crd = 57.295779578552292f;
    deltax = 1.f / (nvparammtstnv2_1.cfac / 1e6f);
    deltay = 1.f / (nvparammtstnv2_1.lfac / 1e6f);
    xr = *xele - nvparammtstnv2_1.coff / 10.f;
    yr = *xlin - nvparammtstnv2_1.loff / 10.f;
    xr = xr * deltax * cdr;
    yr = yr * deltay * cdr;
    tanx = tan(xr);
    tany = tan(yr);
    v1 = tanx * tanx + 1.f;
    v2 = tany * tany * ((a + 1.f) * (a + 1.f)) + 1.f;
    if (v1 * v2 > yk * yk / (yk * yk - 1)) {
	*xlat = -999.f;
	*xlon = -999.f;
	return 0;
    }
    vmu = (rs - re * sqrt(yk * yk - (yk * yk - 1) * v1 * v2)) / (v1 * v2);
    xt = rs - vmu;
    yt = -vmu * tanx;
    zt = vmu * tany / cos(xr);
    teta = asin(zt / rp);
    *xlat = atan(tan(teta) * re / rp) * crd;
    *xlon = atan(yt / xt) * crd + 140.25f;
    return 0;
} /* mtst_to_llmtstnv2__ */


