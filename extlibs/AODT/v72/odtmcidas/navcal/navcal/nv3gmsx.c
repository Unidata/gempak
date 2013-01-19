/* nv3gmsx.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer navtype;
} gmsnavcomgmsxnv3_;

#define gmsnavcomgmsxnv3_1 gmsnavcomgmsxnv3_

/* Table of constant values */

static integer c__504 = 504;
static integer c__508 = 508;
static real c_b9 = 0.f;
static integer c__0 = 0;

integer nv3inigmsx_(integer *ifunc, integer *iparms)
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer i__;
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);
    static char form[5];
    extern /* Subroutine */ int movc_(integer *, integer *, integer *, char *,
	     integer *, ftnlen);
    static char cparms[1*3200];
    static integer offset1, offset2;
    extern /* Subroutine */ int decode_oa_block__(char *, char *, ftnlen, 
	    ftnlen);

    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    if (*ifunc == 1) {
	offset1 = 4;
	offset2 = 0;
	movc_(&c__504, &iparms[1], &offset1, cparms, &offset2, (ftnlen)1);
	offset1 += 508;
	offset2 += 504;
	for (i__ = 1; i__ <= 4; ++i__) {
	    movc_(&c__508, &iparms[1], &offset1, cparms, &offset2, (ftnlen)1);
	    offset1 += 512;
	    offset2 += 508;
/* L100: */
	}
	s_copy(form, "short", (ftnlen)5, (ftnlen)5);
	decode_oa_block__(cparms, form, (ftnlen)1, (ftnlen)5);
    } else if (*ifunc == 2) {
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    gmsnavcomgmsxnv3_1.navtype = 1;
	}
	clit_(ch__1, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__1, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    gmsnavcomgmsxnv3_1.navtype = 2;
	}
    }
    ret_val = 0;
    return ret_val;
} /* nv3inigmsx_ */

integer nv3saegmsx_(real *lin, real *ele, real *dum1, real *lat, real *lon, 
	real *z__)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int sublatlon_(real *);
    static integer mode;
    static doublereal dsct;
    static real rinf[8];
    static integer iret;
    static real ylat, ylon;
    extern /* Subroutine */ int llcart_(real *, real *, real *, real *, real *
	    );
    static real latlon[2], sublat, sublon;
    extern /* Subroutine */ int mgivsr_(integer *, real *, real *, real *, 
	    real *, real *, real *, doublereal *, integer *);

    mode = -1;
    sublatlon_(latlon);
    sublat = latlon[0];
    sublon = latlon[1];
    mgivsr_(&mode, ele, lin, lon, lat, &c_b9, rinf, &dsct, &iret);
    if (iret != 0) {
	goto L100;
    }
    if (abs(*lon) > 180.f) {
	goto L100;
    }
    *lon *= -1.f;
    if (*lon > 90.f - sublon && *lon < 270.f - sublon) {
	goto L100;
    }
    if (gmsnavcomgmsxnv3_1.navtype == 1) {
	ylat = *lat;
	ylon = *lon;
	llcart_(&ylat, &ylon, lat, lon, z__);
    }
    ret_val = 0;
    return ret_val;
L100:
    ret_val = -1;
    return ret_val;
} /* nv3saegmsx_ */

integer nv3easgmsx_(real *inlat, real *inlon, real *z__, real *lin, real *ele,
	 real *dum1)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real x, y;
    extern /* Subroutine */ int sublatlon_(real *);
    static real lat, lon;
    static integer mode;
    static doublereal dsct;
    static real rinf[8];
    static integer iret;
    extern /* Subroutine */ int cartll_(real *, real *, real *, real *, real *
	    );
    static real latlon[2], sublat, sublon;
    extern /* Subroutine */ int mgivsr_(integer *, real *, real *, real *, 
	    real *, real *, real *, doublereal *, integer *);

    lat = *inlat;
    lon = *inlon;
    if (gmsnavcomgmsxnv3_1.navtype == 1) {
	x = lat;
	y = lon;
	cartll_(&x, &y, z__, &lat, &lon);
    }
    mode = 1;
    sublatlon_(latlon);
    sublat = latlon[0];
    sublon = latlon[1];
    if (abs(lat) > 90.f) {
	goto L100;
    }
    if (abs(lon) > 180.f) {
	goto L100;
    }
    if (lon > 90.f - sublon && lon < 270.f - sublon) {
	goto L100;
    }
    lon *= -1.f;
    mgivsr_(&mode, ele, lin, &lon, &lat, &c_b9, rinf, &dsct, &iret);
    if (iret != 0) {
	goto L100;
    }
    ret_val = 0;
    return ret_val;
L100:
    ret_val = -1;
    return ret_val;
} /* nv3easgmsx_ */

integer nv3optgmsx_(integer *ifunc, real *xin, real *xout)
{
    /* System generated locals */
    integer ret_val;
    char ch__1[4];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int sublatlon_(real *);
    extern /* Character */ VOID clit_(char *, ftnlen, integer *);
    static char cfunc[4];
    extern /* Subroutine */ int sdest_(char *, integer *, ftnlen);

    /* Parameter adjustments */
    --xout;
    --xin;

    /* Function Body */
    clit_(ch__1, (ftnlen)4, ifunc);
    s_copy(cfunc, ch__1, (ftnlen)4, (ftnlen)4);
    ret_val = 0;
    if (s_cmp(cfunc, "SPOS", (ftnlen)4, (ftnlen)4) == 0) {
	sublatlon_(&xout[1]);
	sdest_("IN NVX OPT USING --- SPOS", &c__0, (ftnlen)25);
    } else if (s_cmp(cfunc, "ANG ", (ftnlen)4, (ftnlen)4) == 0) {
	sdest_("IN NVX OPT USING --- ANG ", &c__0, (ftnlen)25);
    } else if (s_cmp(cfunc, "HGT ", (ftnlen)4, (ftnlen)4) == 0) {
	sdest_("IN NVX OPT USING --- HGT ", &c__0, (ftnlen)25);
    } else {
	ret_val = 1;
    }
    return ret_val;
} /* nv3optgmsx_ */

integer icon1gmsxnv3_(integer *yymmdd)
{
    /* Initialized data */

    static integer num[12] = { 0,31,59,90,120,151,181,212,243,273,304,334 };

    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer day, year, month, julday;

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
} /* icon1gmsxnv3_ */

/* Subroutine */ int nllxyzgmsxnv3_(real *xlat, real *xlon, real *x, real *y, 
	real *z__)
{
    /* System generated locals */
    real r__1;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), atan2(doublereal, doublereal), 
	    sqrt(doublereal);

    /* Local variables */
    static real r__, ab, asq, bsq, rsq, csln, cslt, ylat, snln, ylon, snlt, 
	    tnlt, rdpdg;

    ab = 40546851.22f;
    asq = 40683833.48f;
    bsq = 40410330.18f;
    r__ = 6371.221f;
    rsq = r__ * r__;
    rdpdg = .01745329252f;
    ylat = rdpdg * *xlat;
    ylat = atan2(bsq * sin(ylat), asq * cos(ylat));
    ylon = -rdpdg * *xlon;
    snlt = sin(ylat);
    cslt = cos(ylat);
    csln = cos(ylon);
    snln = sin(ylon);
/* Computing 2nd power */
    r__1 = snlt / cslt;
    tnlt = r__1 * r__1;
    r__ = ab * sqrt((tnlt + 1.f) / (bsq + asq * tnlt));
    *x = r__ * cslt * csln;
    *y = r__ * cslt * snln;
    *z__ = r__ * snlt;
    return 0;
} /* nllxyzgmsxnv3_ */

/* Subroutine */ int nxyzllgmsxnv3_(real *x, real *y, real *z__, real *xlat, 
	real *xlon)
{
    /* Builtin functions */
    double sqrt(doublereal), atan(doublereal), sin(doublereal), cos(
	    doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    static real a, asq, bsq, rdpdg;

    asq = 40683833.48f;
    bsq = 40410330.18f;
    rdpdg = .01745329252f;
    *xlat = 100.f;
    *xlon = 200.f;
    if (*x == 0.f && *y == 0.f && *z__ == 0.f) {
	goto L90;
    }
    a = atan(*z__ / sqrt(*x * *x + *y * *y));
    *xlat = atan2(asq * sin(a), bsq * cos(a)) / rdpdg;
    *xlon = -atan2(*y, *x) / rdpdg;
L90:
    return 0;
} /* nxyzllgmsxnv3_ */

