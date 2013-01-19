/* nv3merc.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    real xrow, xcol, xlat1, xspace, xqlon, xblat, xblon;
    integer itype, iwest, leftlon;
} mercommercnv3_;

#define mercommercnv3_1 mercommercnv3_

integer nv3inimerc_(ifunc, iparms)
integer *ifunc, *iparms;
{
    /* Initialized data */

    static real rad = (float).01745329;

    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    double cos();
    integer i_indx();

    /* Local variables */
    static doublereal drad;
    extern /* Character */ VOID clit_();
    static real r__;
    extern doublereal flalo_();
    extern /* Subroutine */ int llopt_();
    extern integer lit_();
    static doublereal decc;

    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    if (*ifunc == 1) {
	if (iparms[1] != lit_("MERC", (ftnlen)4)) {
	    ret_val = -1;
	    return ret_val;
	}
	mercommercnv3_1.itype = 1;
	mercommercnv3_1.xrow = (real) iparms[2];
	mercommercnv3_1.xcol = (real) iparms[3];
	mercommercnv3_1.xlat1 = flalo_(&iparms[4]);
	mercommercnv3_1.xspace = iparms[5] / (float)1e3;
	mercommercnv3_1.xqlon = flalo_(&iparms[6]);
	drad = iparms[7] / 1e3;
	r__ = drad;
	decc = iparms[8] / 1e6;
	mercommercnv3_1.iwest = iparms[10];
	if (mercommercnv3_1.iwest >= 0) {
	    mercommercnv3_1.iwest = 1;
	}
	llopt_(&drad, &decc, &mercommercnv3_1.iwest, &iparms[9]);
	mercommercnv3_1.xblat = r__ * cos(mercommercnv3_1.xlat1 * rad) / 
		mercommercnv3_1.xspace;
	mercommercnv3_1.xblon = rad * r__ / mercommercnv3_1.xspace;
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    mercommercnv3_1.itype = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    mercommercnv3_1.itype = 2;
	}
    }
    mercommercnv3_1.leftlon = mercommercnv3_1.xqlon - mercommercnv3_1.iwest * 
	    180;
    ret_val = 0;
    return ret_val;
} /* nv3inimerc_ */

integer nv3saemerc_(xlin, xele, xdum, xlat, xlon, z__)
real *xlin, *xele, *xdum, *xlat, *xlon, *z__;
{
    /* Initialized data */

    static real rad = (float).01745329;

    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double exp(), atan();

    /* Local variables */
    static real ylat, ylon, xedif, xldif, xrlat, xrlon;
    extern /* Subroutine */ int llcart_();

    xldif = mercommercnv3_1.xrow - *xlin;
    xedif = mercommercnv3_1.xcol - *xele;
    xrlon = mercommercnv3_1.iwest * xedif / mercommercnv3_1.xblon;
    *xlon = xrlon + mercommercnv3_1.xqlon;
    xrlat = atan(exp(xldif / mercommercnv3_1.xblat));
    *xlat = (xrlat / rad - (float)45.) * (float)2. + mercommercnv3_1.xlat1;
    if (*xlon > (real) (mercommercnv3_1.leftlon + 360)) {
	goto L20;
    }
    if (*xlon < (real) mercommercnv3_1.leftlon) {
	goto L20;
    }
    if (mercommercnv3_1.itype == 1) {
	ylat = *xlat;
	ylon = *xlon;
	llcart_(&ylat, &ylon, xlat, xlon, z__);
    }
    ret_val = 0;
    return ret_val;
L20:
    ret_val = -1;
    return ret_val;
} /* nv3saemerc_ */

integer nv3easmerc_(zlat, zlon, z__, xlin, xele, xdum)
real *zlat, *zlon, *z__, *xlin, *xele, *xdum;
{
    /* Initialized data */

    static real rad = (float).01745329;

    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    double tan(), log();

    /* Local variables */
    static real xlat, xlon, x, y, xrlat, xrlon;
    extern /* Subroutine */ int cartll_();

    xlat = *zlat;
    xlon = *zlon;
    if (mercommercnv3_1.itype == 1) {
	x = xlat;
	y = xlon;
	cartll_(&x, &y, z__, &xlat, &xlon);
    }
    xrlon = mercommercnv3_1.iwest * (xlon - mercommercnv3_1.xqlon);
    if (xrlon > (float)180.) {
	xrlon += (float)-360.;
    }
    if (xrlon < (float)-180.) {
	xrlon += (float)360.;
    }
    if (xlat >= (float)90.) {
	xlat = (float)89.99;
    }
    if (xlat <= (float)-90.) {
	xlat = (float)-89.99;
    }
    xrlat = ((xlat - mercommercnv3_1.xlat1) / (float)2. + (float)45.) * rad;
    if (xrlat <= (float)0.) {
	ret_val = -1;
	return ret_val;
    }
    *xlin = mercommercnv3_1.xrow - mercommercnv3_1.xblat * log(tan(xrlat));
    *xele = mercommercnv3_1.xcol - xrlon * mercommercnv3_1.xblon;
    ret_val = 0;
    return ret_val;
} /* nv3easmerc_ */

integer nv3optmerc_(ifunc, xin, xout)
integer *ifunc;
real *xin, *xout;
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy();
    integer s_cmp();

    /* Local variables */
    extern /* Character */ VOID clit_();
    static char cfunc[4];
    extern /* Subroutine */ int llobl_();

    /* Parameter adjustments */
    --xout;
    --xin;

    /* Function Body */
    clit_(ch__1, (ftnlen)4, ifunc);
    s_copy(cfunc, ch__1, (ftnlen)4, (ftnlen)4);
    ret_val = 0;
    if (s_cmp(cfunc, "SPOS", (ftnlen)4, (ftnlen)4) == 0) {
	xout[1] = mercommercnv3_1.xlat1;
	xout[2] = mercommercnv3_1.xqlon;
    } else if (s_cmp(cfunc, "ORAD", (ftnlen)4, (ftnlen)4) == 0) {
	llobl_(&xin[1], &xout[1]);
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* nv3optmerc_ */

