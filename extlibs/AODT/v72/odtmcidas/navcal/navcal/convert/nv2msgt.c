/* nv2msgt.f -- translated by f2c (version 20031025).
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
    integer itype;
} msgmsgtnv2_;

#define msgmsgtnv2_1 msgmsgtnv2_

struct {
    integer loff, coff, lfac, cfac;
} nvparammsgtnv2_;

#define nvparammsgtnv2_1 nvparammsgtnv2_

integer nv2inimsgt_(integer *ifunc, integer *iparms)
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
    msgmsgtnv2_1.itype = 0;
    ret_val = 0;
    if (*ifunc == 1) {
	if (iparms[1] != lit_("MSGT", (ftnlen)4)) {
	    ret_val = -1;
	    return ret_val;
	}
	nvparammsgtnv2_1.loff = iparms[2];
	nvparammsgtnv2_1.coff = iparms[3];
	nvparammsgtnv2_1.lfac = iparms[4];
	nvparammsgtnv2_1.cfac = iparms[5];
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    msgmsgtnv2_1.itype = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    msgmsgtnv2_1.itype = 2;
	}
    }
    return ret_val;
} /* nv2inimsgt_ */

integer nv2saemsgt_(real *xlin, real *xele, real *xdum, real *xlat, real *
	xlon, real *z__)
{
    /* System generated locals */
    integer ret_val;
    real r__1, r__2;

    /* Local variables */
    extern /* Subroutine */ int msg_to_llmsgtnv2__(real *, real *, real *, 
	    real *);
    static real a, b;
    extern /* Subroutine */ int nllxyz_(real *, real *, real *, real *, real *
	    );

    ret_val = 0;
    r__1 = 3712.f - (*xlin - 2) / 3.f;
    r__2 = 3712.f - (*xele - 2) / 3.f;
    msg_to_llmsgtnv2__(&r__1, &r__2, xlat, xlon);
    if (*xlat < -900.f) {
	ret_val = -1;
	return ret_val;
    }
    *xlon = -(*xlon);
    if (msgmsgtnv2_1.itype == 1) {
	a = *xlat;
	b = *xlon;
	nllxyz_(&a, &b, xlat, xlon, z__);
    }
    return ret_val;
} /* nv2saemsgt_ */

integer nv2easmsgt_(real *xlat, real *xlon, real *z__, real *xlin, real *xele,
	 real *xdum)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int ll_to_msgmsgtnv2__(real *, real *, real *, 
	    real *);
    static real a, b;
    extern /* Subroutine */ int nxyzll_(real *, real *, real *, real *, real *
	    );

    ret_val = 0;
    a = *xlat;
    b = -(*xlon);
    if (msgmsgtnv2_1.itype == 1) {
	nxyzll_(xlat, xlon, z__, &a, &b);
	b = -b;
    }
    ll_to_msgmsgtnv2__(&a, &b, xlin, xele);
    if (*xlin < 0.f) {
	ret_val = -1;
	return ret_val;
    }
    *xlin = 11136 - ((*xlin - 2) * 3.f - 1.5f);
    *xele = 11136 - ((*xele - 2) * 3.f - 1.5f);
    *xlin += -1;
    *xele += -1;
    return ret_val;
} /* nv2easmsgt_ */

integer nv2optmsgt_(integer *ifunc, real *xin, real *xout)
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
	xout[2] = 0.f;
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* nv2optmsgt_ */

/* Subroutine */ int ll_to_msgmsgtnv2__(real *xlat, real *xlon, real *xlin, 
	real *xele)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal), tan(doublereal)
	    , atan(doublereal);

    /* Local variables */
    static real a, h__, y, r1, r2, re, rp, rs, px, py, xr, yr, xt, yt, zt, 
	    cdr, crd, xla, xfi, rom, teta, deltax, deltay;


/*       S/R gives line and element for */
/*       a specified MSG latitude and longitude */

/*       Inputs: */
/*       xlat,xlon (REAL)  : latitude and longitude of selected point */
/*                           latitude North is positive, */
/*                           longitude East is positive */

/*       Outputs: */
/*       xlin, xele (REAL) : line and element number */
/*                           assumes that line 1 is in the South */
/*                           and element 1 is in the East */
/*                           based on 3712 lines and elements in total */
/*                           Output is -999. if specified xlat/xlon is */
/*                           not within MSG field-of-view */

/*       Subroutine assumes that line 1856.5 and element 1856.5 is 0/0 deg */

    re = 6378.155f;
    h__ = 42164.f - re;
    rs = re + h__;
    a = .0033670033670033669f;
    rp = re / (a + 1.f);
    cdr = .017453292500000002f;
    crd = 57.295779578552292f;
    deltax = 1.f / (nvparammsgtnv2_1.cfac / 1e6f);
    deltay = 1.f / (nvparammsgtnv2_1.lfac / 1e6f);
    xfi = *xlat * cdr;
    xla = *xlon * cdr;
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
    *xele = nvparammsgtnv2_1.coff / 10.f - xr;
    *xlin = nvparammsgtnv2_1.loff / 10.f - yr;
    *xele = 3713.f - *xele;
    *xlin = 3713.f - *xlin;
    return 0;
} /* ll_to_msgmsgtnv2__ */

/* Subroutine */ int msg_to_llmsgtnv2__(real *xlin, real *xele, real *xlat, 
	real *xlon)
{
    /* Builtin functions */
    double tan(doublereal), sqrt(doublereal), cos(doublereal), asin(
	    doublereal), atan(doublereal);

    /* Local variables */
    static real a, h__, v1, v2, re, rp, yk, rs, xr, yr, xt, yt, zt, cdr, crd, 
	    vmu, teta, tanx, tany, deltax, deltay;


/*       S/R gives latitude and longitude for */
/*       a specified MSG line and element */

/*       Inputs: */
/*       xlin, xele (REAL) : line and element number */
/*                           assumes that line 1 is in the South */
/*                           and element 1 is in the East */
/*                           based on 3712 lines and elements in total */

/*       Outputs: */
/*       xlat,xlon (REAL)  : latitude and longitude of selected point */
/*                           latitude North is positive, */
/*                           longitude East is positive */
/*                           output is -999. if line/element is off the disk */

/*       Subroutine assumes that line 1856.5 and element 1856.5 is 0/0 deg */

    re = 6378.155f;
    h__ = 42164.f - re;
    rs = re + h__;
    yk = rs / re;
    a = .0033670033670033669f;
    rp = re / (a + 1.f);
    cdr = .017453292500000002f;
    crd = 57.295779578552292f;
    deltax = 1.f / (nvparammsgtnv2_1.cfac / 1e6f);
    deltay = 1.f / (nvparammsgtnv2_1.lfac / 1e6f);
    xr = *xele - nvparammsgtnv2_1.coff / 10.f;
    yr = *xlin - nvparammsgtnv2_1.loff / 10.f;
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
    *xlon = atan(yt / xt) * crd;
    return 0;
} /* msg_to_llmsgtnv2__ */

