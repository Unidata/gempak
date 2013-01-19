/* nv2gvar.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal xs[3], bt[9]	/* was [3][3] */, q3, pitch, roll, yaw;
    real pma, rma;
} elcommgvarnv2_;

#define elcommgvarnv2_1 elcommgvarnv2_

struct {
    integer incmax[2];
    real elvmax[2], scnmax[2], elvinc[2], scninc[2], elvln[2], scnpx[2], 
	    ewnom[2], nsnom[2];
} instcogvarnv2_;

#define instcogvarnv2_1 instcogvarnv2_

struct {
    integer itype, instr;
    doublereal sublat, sublon;
    integer iflip;
} gvrcomgvarnv2_;

#define gvrcomgvarnv2_1 gvrcomgvarnv2_

struct {
    doublereal aec, ferc, aebe2c, aebe3c, aebe4c;
} radcomgvarnv2_;

#define radcomgvarnv2_1 radcomgvarnv2_

struct {
    doublereal b[9]	/* was [3][3] */, dr, phi;
} savcomgvarnv2_;

#define savcomgvarnv2_1 savcomgvarnv2_

/* Table of constant values */

static integer c__0 = 0;
static integer c__63 = 63;
static integer c__130 = 130;
static integer c__185 = 185;
static integer c__258 = 258;
static integer c__313 = 313;
static real c_b22 = (float)360.;
static integer c__1 = 1;

integer nv2inigvar_(ifunc, iparms)
integer *ifunc, *iparms;
{
    /* Initialized data */

    static struct {
	integer e_1[31];
	integer fill_2[9];
	integer e_3[31];
	integer fill_4[9];
	} equiv_44 = { 5, 14, 66, 99, 104, 109, 114, 117, 121, 154, 159, 164, 
		169, 172, 176, 209, 214, 219, 224, 227, 231, 264, 269, 274, 
		279, 286, 319, 324, 329, 334, -1, {0}, 11, 64, 95, 101, 106, 
		111, 116, 119, 150, 156, 161, 166, 171, 174, 205, 211, 216, 
		221, 226, 229, 260, 266, 271, 276, 284, 315, 321, 326, 331, 
		336, -1 };

#define rellst ((integer *)&equiv_44)


    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1, d__2, d__3;
    char ch__1[12], ch__2[4];
    static real equiv_0[640];

    /* Builtin functions */
    double sin(), cos(), sqrt(), atan2(), tan(), atan();
    integer i_indx();

    /* Local variables */
    static doublereal dlat;
    extern /* Character */ VOID clit_();
    static doublereal secs;
    static integer time[2], year;
    static doublereal slat, dyaw, cosu;
    extern doublereal time50gvarnv2_();
    static integer loop, hour;
    static doublereal sinu, syaw;
    extern doublereal timexgvarnv2_();
    extern /* Subroutine */ int inst2egvarnv2_();
    static doublereal r__, u, w, epoch;
    extern /* Subroutine */ int ddest_();
    static doublereal cosoi;
    extern integer iftok_();
    static doublereal sinoi;
    static integer count;
    extern /* Subroutine */ int setcongvarnv2_();
#define iparm2 ((integer *)equiv_0)
    static doublereal wa, te, cw, ts, sinasc, cosasc, sw, c2w, cw1, cw3;
#define rparms (equiv_0)
    static doublereal imgtim, sw1, s2w, sw3;
    static integer minuts, offset;
    static doublereal asc;
    static integer imc;
    static doublereal lam;
    static integer day;
    extern /* Character */ VOID cfz_();
    extern integer lit_();
    static doublereal psi;
    extern doublereal gattgvarnv2_();

    /* Parameter adjustments */
    --iparms;

    /* Function Body */
    radcomgvarnv2_1.aec = 6378.137;
    radcomgvarnv2_1.ferc = .0033526561125920562;
    radcomgvarnv2_1.aebe2c = 1.0067391845079681;
    radcomgvarnv2_1.aebe3c = .0067391845079680657;
    radcomgvarnv2_1.aebe4c = -.013343333245450451;
    if (*ifunc == 1) {
	if (iparms[1] != lit_("GVAR", (ftnlen)4)) {
	    ret_val = -1;
	    return ret_val;
	}
	gvrcomgvarnv2_1.itype = 1;
	for (loop = 1; loop <= 640; ++loop) {
/* L1: */
	    iparm2[loop - 1] = iparms[loop];
	}
	count = 1;
	rparms[368] = (real) iparm2[368] / (float)1e3;
L2:
	if (rellst[count - 1] == -1) {
	    goto L4;
	}
	offset = 1;
	if (rellst[count - 1] > 116) {
	    offset = 13;
	}
	if (rellst[count - 1] > 226) {
	    offset = 31;
	}
	i__1 = rellst[count + 39];
	for (loop = rellst[count - 1]; loop <= i__1; ++loop) {
	    if (loop == 14 || loop == 61 || (loop - 8) % 55 == 0 && loop != 8)
		     {
		rparms[loop + offset - 1] = (real) iparm2[loop + offset - 1] /
			 (float)100.;
	    } else {
		rparms[loop + offset - 1] = (real) iparm2[loop + offset - 1] /
			 (float)1e7;
	    }
/* L3: */
	}
	++count;
	goto L2;
L4:
	gvrcomgvarnv2_1.instr = iparms[370];
	setcongvarnv2_(&gvrcomgvarnv2_1.instr, &iparms[380], &iparms[382], &
		iparms[381], &iparms[383]);
	year = iparms[368] / 1000 + 1900;
	day = iparms[368] - iparms[368] / 1000 * 1000;
	hour = rparms[368] / 10000;
	minuts = rparms[368] / 100 - hour * 100;
	secs = rparms[368] - (real) (minuts * 100) - (real) (hour * 10000);
	imgtim = timexgvarnv2_(&year, &day, &hour, &minuts, &secs);
	cfz_(ch__1, (ftnlen)12, &iparms[13]);
	time[0] = iftok_(ch__1, (ftnlen)12);
	cfz_(ch__1, (ftnlen)12, &iparms[14]);
	time[1] = iftok_(ch__1, (ftnlen)12);
	epoch = time50gvarnv2_(time);
	imc = 1;
	gvrcomgvarnv2_1.iflip = 1;
	if (bit_test(iparms[3],7)) {
	    imc = 0;
	}
	if (bit_test(iparms[4],15)) {
	    gvrcomgvarnv2_1.iflip = -1;
	}
	ddest_("IFLIP is set to", &gvrcomgvarnv2_1.iflip, (ftnlen)15);
	if (gvrcomgvarnv2_1.iflip == -1) {
	    ddest_("USING FLIPPED NAV", &c__0, (ftnlen)17);
	}
	lam = rparms[5];
	savcomgvarnv2_1.dr = rparms[6];
	savcomgvarnv2_1.phi = rparms[7];
	psi = rparms[8];
	elcommgvarnv2_1.roll = rparms[9];
	elcommgvarnv2_1.pitch = rparms[10];
	elcommgvarnv2_1.yaw = rparms[11];
	elcommgvarnv2_1.rma = (float)0.;
	elcommgvarnv2_1.pma = (float)0.;
	if (imc != 0) {
	    savcomgvarnv2_1.dr = (float)0.;
	    savcomgvarnv2_1.phi = (float)0.;
	    psi = (float)0.;
	    ts = imgtim - epoch;
	    w = ts * .0043746900000000005;
	    sw = sin(w);
	    cw = cos(w);
	    sw1 = sin(w * (float).927);
	    cw1 = cos(w * (float).927);
	    s2w = sin(w * (float)2.);
	    c2w = cos(w * (float)2.);
	    sw3 = sin(w * (float)1.9268);
	    cw3 = cos(w * (float)1.9268);
	    lam = lam + rparms[18] + (rparms[19] + rparms[20] * w) * w + (
		    rparms[27] * sw1 + rparms[28] * cw1 + rparms[21] * sw + 
		    rparms[22] * cw + rparms[23] * s2w + rparms[24] * c2w + 
		    rparms[25] * sw3 + rparms[26] * cw3 + w * (rparms[29] * 
		    sw + rparms[30] * cw)) * (float)2.;
	    savcomgvarnv2_1.dr = savcomgvarnv2_1.dr + rparms[31] + rparms[32] 
		    * cw + rparms[33] * sw + rparms[34] * c2w + rparms[35] * 
		    s2w + rparms[36] * cw3 + rparms[37] * sw3 + rparms[38] * 
		    cw1 + rparms[39] * sw1 + w * (rparms[40] * cw + rparms[41]
		     * sw);
	    dlat = rparms[42] + rparms[43] * cw + rparms[44] * sw + rparms[45]
		     * c2w + rparms[46] * s2w + w * (rparms[47] * cw + rparms[
		    48] * sw) + rparms[49] * cw1 + rparms[50] * sw1;
	    savcomgvarnv2_1.phi += dlat * (dlat * dlat / (float)6. + (float)
		    1.);
	    dyaw = rparms[51] + rparms[52] * sw + rparms[53] * cw + rparms[54]
		     * s2w + rparms[55] * c2w + w * (rparms[56] * sw + rparms[
		    57] * cw) + rparms[58] * sw1 + rparms[59] * cw1;
	    psi += dyaw * (dyaw * dyaw / (float)6. + (float)1.);
	}
	slat = sin(savcomgvarnv2_1.phi);
	syaw = sin(psi);
/* Computing 2nd power */
	d__1 = slat;
/* Computing 2nd power */
	d__2 = syaw;
	sinoi = d__1 * d__1 + d__2 * d__2;
	cosoi = sqrt((float)1. - sinoi);
	sinoi = sqrt(sinoi);
	if (slat == 0. && syaw == 0.) {
	    u = 0.;
	} else {
	    u = atan2(slat, syaw);
	}
	sinu = sin(u);
	cosu = cos(u);
	asc = lam - u;
	sinasc = sin(asc);
	cosasc = cos(asc);
	gvrcomgvarnv2_1.sublat = atan(radcomgvarnv2_1.aebe2c * tan(
		savcomgvarnv2_1.phi));
	gvrcomgvarnv2_1.sublon = asc + atan2(cosoi * sinu, cosu);
	savcomgvarnv2_1.b[3] = -sinasc * sinoi;
	savcomgvarnv2_1.b[4] = cosasc * sinoi;
	savcomgvarnv2_1.b[5] = -cosoi;
	savcomgvarnv2_1.b[6] = -cosasc * cosu + sinasc * sinu * cosoi;
	savcomgvarnv2_1.b[7] = -sinasc * cosu - cosasc * sinu * cosoi;
	savcomgvarnv2_1.b[8] = -slat;
	savcomgvarnv2_1.b[0] = -cosasc * sinu - sinasc * cosu * cosoi;
	savcomgvarnv2_1.b[1] = -sinasc * sinu + cosasc * cosu * cosoi;
	savcomgvarnv2_1.b[2] = cosu * sinoi;
	r__ = (savcomgvarnv2_1.dr + 42164.365) / radcomgvarnv2_1.aec;
	elcommgvarnv2_1.xs[0] = -savcomgvarnv2_1.b[6] * r__;
	elcommgvarnv2_1.xs[1] = -savcomgvarnv2_1.b[7] * r__;
	elcommgvarnv2_1.xs[2] = -savcomgvarnv2_1.b[8] * r__;
/* Computing 2nd power */
	d__1 = elcommgvarnv2_1.xs[0];
/* Computing 2nd power */
	d__2 = elcommgvarnv2_1.xs[1];
/* Computing 2nd power */
	d__3 = elcommgvarnv2_1.xs[2];
	elcommgvarnv2_1.q3 = d__1 * d__1 + d__2 * d__2 + 
		radcomgvarnv2_1.aebe2c * (d__3 * d__3) - (float)1.;
	if (imc != 0) {
	    wa = rparms[60] * ts;
	    te = ts - rparms[61];
	    elcommgvarnv2_1.roll += gattgvarnv2_(&c__63, rparms, &iparms[1], &
		    wa, &te);
	    elcommgvarnv2_1.pitch += gattgvarnv2_(&c__130, rparms, &iparms[1],
		     &wa, &te);
	    elcommgvarnv2_1.yaw += gattgvarnv2_(&c__185, rparms, &iparms[1], &
		    wa, &te);
	    elcommgvarnv2_1.rma = gattgvarnv2_(&c__258, rparms, &iparms[1], &
		    wa, &te);
	    elcommgvarnv2_1.pma = gattgvarnv2_(&c__313, rparms, &iparms[1], &
		    wa, &te);
	    elcommgvarnv2_1.roll += rparms[15];
	    elcommgvarnv2_1.pitch += rparms[16];
	    elcommgvarnv2_1.yaw += rparms[17];
	}
	inst2egvarnv2_(&elcommgvarnv2_1.roll, &elcommgvarnv2_1.pitch, &
		elcommgvarnv2_1.yaw, savcomgvarnv2_1.b, elcommgvarnv2_1.bt);
    } else if (*ifunc == 2) {
	clit_(ch__2, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__2, "LL", (ftnlen)4, (ftnlen)2) != 0) {
	    gvrcomgvarnv2_1.itype = 1;
	}
	clit_(ch__2, (ftnlen)4, &iparms[1]);
	if (i_indx(ch__2, "XY", (ftnlen)4, (ftnlen)2) != 0) {
	    gvrcomgvarnv2_1.itype = 2;
	}
    }
    ret_val = 0;
    return ret_val;
} /* nv2inigvar_ */

#undef rparms
#undef iparm2
#undef rellst


integer nv2saegvar_(xlin, xele, xdum, xlat, xlon, z__)
real *xlin, *xele, *xdum, *xlat, *xlon, *z__;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static real ylat;
    static integer stat;
    static real ylon;
    static doublereal e, s;
    extern /* Subroutine */ int lpointgvarnv2_();
    static doublereal rl, rp;
    extern /* Subroutine */ int llcart_();
    static doublereal tmplat, tmplon;
    extern doublereal evlngvarnv2_(), scpxgvarnv2_();

    rl = *xlin;
    rp = *xele;
    if (gvrcomgvarnv2_1.instr == 2) {
	rl = (rl + (float)9.) / (float)10.;
	rp = (rp + (float)9.) / (float)10.;
    }
    e = evlngvarnv2_(&gvrcomgvarnv2_1.instr, &rl);
    s = scpxgvarnv2_(&gvrcomgvarnv2_1.instr, &rp);
    lpointgvarnv2_(&gvrcomgvarnv2_1.instr, &gvrcomgvarnv2_1.iflip, &e, &s, &
	    tmplat, &tmplon, &stat);
    if (stat != 0) {
	goto L900;
    }
    tmplat *= 57.295779513082323;
    tmplon *= 57.295779513082323;
    tmplon = -tmplon;
    if (gvrcomgvarnv2_1.itype == 2) {
	ylat = tmplat;
	ylon = tmplon;
	llcart_(&ylat, &ylon, xlat, xlon, z__);
    } else {
	*xlat = tmplat;
	*xlon = tmplon;
    }
    ret_val = 0;
    return ret_val;
L900:
    *xlat = tmplat;
    *xlon = -tmplon;
    ret_val = -1;
    return ret_val;
} /* nv2saegvar_ */

integer nv2easgvar_(zlat, zlon, z__, xlin, xele, xdum)
real *zlat, *zlon, *z__, *xlin, *xele, *xdum;
{
    /* System generated locals */
    integer ret_val;
    doublereal d__1, d__2;

    /* Local variables */
    extern /* Subroutine */ int evsc2lgvarnv2_();
    static doublereal e, s;
    static real x, y;
    extern /* Subroutine */ int gpointgvarnv2_(), cartll_();
    static doublereal tmpele, tmplat, tmplin, tmplon;
    static integer ier;

    ret_val = 0;
    if (gvrcomgvarnv2_1.itype == 2) {
	x = *zlat;
	y = *zlon;
	cartll_(&x, &y, z__, zlat, zlon);
    }
    if (dabs(*zlat) > (float)90.) {
	ret_val = -1;
	return ret_val;
    }
    tmplat = *zlat;
    tmplon = *zlon;
    d__1 = tmplat * .017453292519943295;
    d__2 = -tmplon * .017453292519943295;
    gpointgvarnv2_(&gvrcomgvarnv2_1.instr, &gvrcomgvarnv2_1.iflip, &d__1, &
	    d__2, &e, &s, &ier);
    if (ier != 0) {
	ret_val = -1;
	return ret_val;
    }
    evsc2lgvarnv2_(&gvrcomgvarnv2_1.instr, &e, &s, &tmplin, &tmpele);
    *xlin = tmplin;
    *xele = tmpele;
    if (gvrcomgvarnv2_1.instr == 2) {
	*xlin = *xlin * (float)10. - (float)9.;
	*xele = *xele * (float)10. - (float)9.;
    }
    return ret_val;
} /* nv2easgvar_ */

integer nv2optgvar_(ifunc, xin, xout)
integer *ifunc;
real *xin, *xout;
{
    /* Initialized data */

    static integer lasday = -1;
    static integer lastim = -1;

    /* System generated locals */
    integer ret_val;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double r_mod(), tan(), atan();

    /* Local variables */
    static real flat;
    static integer jday;
    static real flon, xlat, xlon;
    static doublereal r__;
    static integer jtime;
    extern /* Subroutine */ int gvranggvarnv2_();
    extern integer iround_();
    extern /* Subroutine */ int solarp_();
    extern integer m0itime_();
    static real dec, gha;
    extern integer lit_();

    /* Parameter adjustments */
    --xout;
    --xin;

    /* Function Body */
    ret_val = -1;
    if (*ifunc == lit_("SPOS", (ftnlen)4)) {
	xout[1] = gvrcomgvarnv2_1.sublat * 57.295779513082323;
	xout[2] = -gvrcomgvarnv2_1.sublon * 57.295779513082323;
	xout[2] = r_mod(&xout[2], &c_b22);
	ret_val = 0;
    } else if (*ifunc == lit_("HGT ", (ftnlen)4)) {
	radcomgvarnv2_1.aec = (doublereal) xin[1] + 6378.137;
	radcomgvarnv2_1.ferc = 1. - ((doublereal) xin[1] + 6356.7533) / 
		radcomgvarnv2_1.aec;
	radcomgvarnv2_1.aebe2c = 1. / ((1. - radcomgvarnv2_1.ferc) * (1. - 
		radcomgvarnv2_1.ferc));
	radcomgvarnv2_1.aebe3c = radcomgvarnv2_1.aebe2c - 1.;
	radcomgvarnv2_1.aebe4c = (1. - radcomgvarnv2_1.ferc) * (1. - 
		radcomgvarnv2_1.ferc) * (1. - radcomgvarnv2_1.ferc) * (1. - 
		radcomgvarnv2_1.ferc) - 1.;
	gvrcomgvarnv2_1.sublat = atan(radcomgvarnv2_1.aebe2c * tan(
		savcomgvarnv2_1.phi));
	r__ = (savcomgvarnv2_1.dr + 42164.365) / radcomgvarnv2_1.aec;
	elcommgvarnv2_1.xs[0] = -savcomgvarnv2_1.b[6] * r__;
	elcommgvarnv2_1.xs[1] = -savcomgvarnv2_1.b[7] * r__;
	elcommgvarnv2_1.xs[2] = -savcomgvarnv2_1.b[8] * r__;
/* Computing 2nd power */
	d__1 = elcommgvarnv2_1.xs[0];
/* Computing 2nd power */
	d__2 = elcommgvarnv2_1.xs[1];
/* Computing 2nd power */
	d__3 = elcommgvarnv2_1.xs[2];
	elcommgvarnv2_1.q3 = d__1 * d__1 + d__2 * d__2 + 
		radcomgvarnv2_1.aebe2c * (d__3 * d__3) - (float)1.;
	ret_val = 0;
    } else if (*ifunc == lit_("ANG ", (ftnlen)4)) {
	jday = iround_(&xin[1]);
	jtime = m0itime_(&xin[2]);
	flat = xin[3];
	flon = xin[4];
	if (jday != lasday || jtime != lastim) {
	    solarp_(&jday, &jtime, &gha, &dec, &xlat, &xlon);
	    lasday = jday;
	    lastim = jtime;
	}
	gvranggvarnv2_(&jday, &jtime, &flat, &flon, &gha, &dec, &xout[1], &
		xout[2], &xout[3]);
	ret_val = 0;
    }
    return ret_val;
} /* nv2optgvar_ */

/* Subroutine */ int setcongvarnv2_(instr, nadnsc, nadnsi, nadewc, nadewi)
integer *instr, *nadnsc, *nadnsi, *nadewc, *nadewi;
{
    instcogvarnv2_1.incmax[0] = 6136;
    instcogvarnv2_1.incmax[1] = 2805;
    instcogvarnv2_1.elvinc[0] = (float)8e-6;
    instcogvarnv2_1.elvinc[1] = (float)1.75e-5;
    instcogvarnv2_1.scninc[0] = (float)1.6e-5;
    instcogvarnv2_1.scninc[1] = (float)3.5e-5;
    instcogvarnv2_1.elvln[0] = (float)2.8e-5;
    instcogvarnv2_1.elvln[1] = (float)2.8e-4;
    instcogvarnv2_1.scnpx[0] = (float)1.6e-5;
    instcogvarnv2_1.scnpx[1] = (float)2.8e-4;
    instcogvarnv2_1.elvmax[0] = (float).220896;
    instcogvarnv2_1.elvmax[1] = (float).22089375;
    instcogvarnv2_1.scnmax[0] = (float).24544;
    instcogvarnv2_1.scnmax[1] = (float).2454375;
    instcogvarnv2_1.nsnom[0] = instcogvarnv2_1.incmax[0] * (float)4.5 * 
	    instcogvarnv2_1.elvinc[0];
    instcogvarnv2_1.nsnom[1] = instcogvarnv2_1.incmax[1] * (float)4.5 * 
	    instcogvarnv2_1.elvinc[1];
    instcogvarnv2_1.ewnom[0] = instcogvarnv2_1.incmax[0] * (float)2.5 * 
	    instcogvarnv2_1.scninc[0];
    instcogvarnv2_1.ewnom[1] = instcogvarnv2_1.incmax[1] * (float)2.5 * 
	    instcogvarnv2_1.scninc[1];
    if (*nadnsc != 0 && *nadnsi != 0 && *nadewc != 0 && *nadewi != 0) {
	if (*instr == 1) {
	    instcogvarnv2_1.elvmax[*instr - 1] = (*nadnsc * 
		    instcogvarnv2_1.incmax[*instr - 1] + *nadnsi) * 
		    instcogvarnv2_1.elvinc[*instr - 1];
	} else {
	    instcogvarnv2_1.elvmax[*instr - 1] = ((9 - *nadnsc) * 
		    instcogvarnv2_1.incmax[*instr - 1] - *nadnsi) * 
		    instcogvarnv2_1.elvinc[*instr - 1];
	}
	instcogvarnv2_1.scnmax[*instr - 1] = (*nadewc * 
		instcogvarnv2_1.incmax[*instr - 1] + *nadewi) * 
		instcogvarnv2_1.scninc[*instr - 1];
    }
    return 0;
} /* setcongvarnv2_ */

doublereal timexgvarnv2_(ny, nd, nh, nm, s)
integer *ny, *nd, *nh, *nm;
doublereal *s;
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer j;

    j = *nd + (*ny + 4799) * 1461 / 4 - (*ny + 4899) / 100 * 3 / 4 - 2465022;
    ret_val = j * 1440. + *nh * 60. + *nm + *s / 60.;
    return ret_val;
} /* timexgvarnv2_ */

doublereal time50gvarnv2_(i__)
integer *i__;
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer j;
    static doublereal s;
    static integer nd, nh, nm, ny, iaa, iab, iac, def, nbc;

    /* Parameter adjustments */
    --i__;

    /* Function Body */
    ny = i__[1] / 10000;
    iaa = i__[1] - ny * 10000;
    nd = (i__[1] - ny * 10000) * (float).1;
    iab = (iaa - nd * 10) * 10;
    nbc = i__[2] / (float)1e7;
    iac = i__[2] - nbc * 10000000;
    nh = iab + nbc;
    def = i__[2] - iac;
    nm = iac * (float)1e-5;
    s = (i__[2] - (def + nm * 100000)) * (float).001;
    j = nd + (ny + 4799) * 1461 / 4 - (ny + 4899) / 100 * 3 / 4 - 2465022;
    ret_val = j * 1440. + nh * 60. + nm + s / 60.;
    return ret_val;
} /* time50gvarnv2_ */

doublereal gattgvarnv2_(k0, rparms, iparms, wa, te)
integer *k0;
real *rparms;
integer *iparms;
doublereal *wa, *te;
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double exp();
    integer i_dnnt();
    double cos(), pow_dd();

    /* Local variables */
    static integer i__, j, k, l, m, ll;
    static doublereal ir, jr, mr;
    static integer kkk;
    static doublereal att;

    /* Parameter adjustments */
    --iparms;
    --rparms;

    /* Function Body */
    k = *k0;
    att = rparms[k + 2];
    if (*te >= 0.) {
	att += rparms[k] * exp(-(*te) / rparms[k + 1]);
    }
    ir = (real) iparms[k + 3];
    i__ = i_dnnt(&ir);
    i__1 = i__;
    for (l = 1; l <= i__1; ++l) {
	att += rparms[k + (l << 1) + 2] * cos(*wa * l + rparms[k + (l << 1) + 
		3]);
/* L10: */
    }
    k += 34;
    ir = (real) iparms[k];
    kkk = iparms[k];
    i__1 = kkk;
    for (l = 1; l <= i__1; ++l) {
	ll = k + l * 5;
	jr = (real) iparms[ll - 4];
	mr = (real) iparms[ll - 3];
	j = i_dnnt(&jr);
	m = i_dnnt(&mr);
	d__1 = *wa - rparms[ll];
	att += rparms[ll - 2] * pow_dd(&d__1, &mr) * cos(jr * *wa + rparms[ll 
		- 1]);
/* L20: */
    }
    ret_val = att;
    return ret_val;
} /* gattgvarnv2_ */

/* Subroutine */ int evsc2lgvarnv2_(instr, elev, scan, rl, rp)
integer *instr;
doublereal *elev, *scan, *rl, *rp;
{
    *rl = (instcogvarnv2_1.elvmax[*instr - 1] - *elev) / 
	    instcogvarnv2_1.elvln[*instr - 1];
    if (*instr == 1) {
	*rl += (float)4.5;
    } else {
	*rl += (float)2.5;
    }
    *rp = (instcogvarnv2_1.scnmax[*instr - 1] + *scan) / 
	    instcogvarnv2_1.scnpx[*instr - 1] + (float)1.;
    return 0;
} /* evsc2lgvarnv2_ */

doublereal evlngvarnv2_(instr, rline)
integer *instr;
doublereal *rline;
{
    /* System generated locals */
    doublereal ret_val;

    if (*instr == 1) {
	ret_val = instcogvarnv2_1.elvmax[*instr - 1] - (*rline - (float)4.5) *
		 instcogvarnv2_1.elvln[*instr - 1];
    } else {
	ret_val = instcogvarnv2_1.elvmax[*instr - 1] - (*rline - (float)2.5) *
		 instcogvarnv2_1.elvln[*instr - 1];
    }
    return ret_val;
} /* evlngvarnv2_ */

doublereal scpxgvarnv2_(instr, pix)
integer *instr;
doublereal *pix;
{
    /* System generated locals */
    doublereal ret_val;

    ret_val = (*pix - (float)1.) * instcogvarnv2_1.scnpx[*instr - 1] - 
	    instcogvarnv2_1.scnmax[*instr - 1];
    return ret_val;
} /* scpxgvarnv2_ */

/* Subroutine */ int inst2egvarnv2_(r__, p, y, a, at)
doublereal *r__, *p, *y, *a, *at;
{
    static integer i__, j;
    static doublereal rpy[9]	/* was [3][3] */;

    /* Parameter adjustments */
    at -= 4;
    a -= 4;

    /* Function Body */
    rpy[0] = (float)1. - (*p * *p + *y * *y) * (float).5;
    rpy[3] = -(*y);
    rpy[6] = *p;
    rpy[1] = *y + *p * *r__;
    rpy[4] = (float)1. - (*y * *y + *r__ * *r__) * (float).5;
    rpy[7] = -(*r__);
    rpy[2] = -(*p) + *r__ * *y;
    rpy[5] = *r__ + *p * *y;
    rpy[8] = (float)1. - (*p * *p + *r__ * *r__) * (float).5;
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    at[i__ + j * 3] = a[i__ + 3] * rpy[j * 3 - 3] + a[i__ + 6] * rpy[
		    j * 3 - 2] + a[i__ + 9] * rpy[j * 3 - 1];
/* L10: */
	}
/* L20: */
    }
    return 0;
} /* inst2egvarnv2_ */

/* Subroutine */ int lpointgvarnv2_(instr, flip_flg__, alpha0, zeta0, rlat, 
	rlon, ierr)
integer *instr, *flip_flg__;
doublereal *alpha0, *zeta0, *rlat, *rlon;
integer *ierr;
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double cos(), sin(), tan(), sqrt(), atan(), atan2();

    /* Local variables */
    static doublereal doff, zeta, d__, g[3], h__, alpha, u[3], d1, g1[3], q1, 
	    q2, ca, da, ff, sa, cz, dz;

    *ierr = 1;
    ff = (doublereal) (*flip_flg__);
    if (*instr == 2) {
	ff = -ff;
    }
    doff = instcogvarnv2_1.scnmax[*instr - 1] - instcogvarnv2_1.ewnom[*instr 
	    - 1];
    alpha = *alpha0 - *alpha0 * *zeta0 * doff;
    zeta = *zeta0 + *alpha0 * (float).5 * *alpha0 * doff;
    ca = cos(alpha);
    sa = sin(alpha);
    cz = cos(zeta);
    da = alpha - elcommgvarnv2_1.pma * sa * (ff / cz + tan(zeta)) - 
	    elcommgvarnv2_1.rma * (1. - ca / cz);
    dz = zeta + ff * elcommgvarnv2_1.rma * sa;
    cz = cos(dz);
    g[0] = sin(dz);
    g[1] = -cz * sin(da);
    g[2] = cz * cos(da);
    g1[0] = elcommgvarnv2_1.bt[0] * g[0] + elcommgvarnv2_1.bt[3] * g[1] + 
	    elcommgvarnv2_1.bt[6] * g[2];
    g1[1] = elcommgvarnv2_1.bt[1] * g[0] + elcommgvarnv2_1.bt[4] * g[1] + 
	    elcommgvarnv2_1.bt[7] * g[2];
    g1[2] = elcommgvarnv2_1.bt[2] * g[0] + elcommgvarnv2_1.bt[5] * g[1] + 
	    elcommgvarnv2_1.bt[8] * g[2];
/* Computing 2nd power */
    d__1 = g1[0];
/* Computing 2nd power */
    d__2 = g1[1];
/* Computing 2nd power */
    d__3 = g1[2];
    q1 = d__1 * d__1 + d__2 * d__2 + radcomgvarnv2_1.aebe2c * (d__3 * d__3);
    q2 = elcommgvarnv2_1.xs[0] * g1[0] + elcommgvarnv2_1.xs[1] * g1[1] + 
	    radcomgvarnv2_1.aebe2c * elcommgvarnv2_1.xs[2] * g1[2];
    d__ = q2 * q2 - q1 * elcommgvarnv2_1.q3;
    if (abs(d__) < 1e-9) {
	d__ = (float)0.;
    }
    if (d__ < 0.) {
	*rlat = (float)999999.;
	*rlon = (float)999999.;
	return 0;
    }
    d__ = sqrt(d__);
    h__ = -(q2 + d__) / q1;
    u[0] = elcommgvarnv2_1.xs[0] + h__ * g1[0];
    u[1] = elcommgvarnv2_1.xs[1] + h__ * g1[1];
    u[2] = elcommgvarnv2_1.xs[2] + h__ * g1[2];
/* Computing 2nd power */
    d__1 = u[0];
/* Computing 2nd power */
    d__2 = u[1];
/* Computing 2nd power */
    d__3 = u[2];
    d1 = u[2] / sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
    *rlat = atan(radcomgvarnv2_1.aebe2c * d1 / sqrt((float)1. - d1 * d1));
    *rlon = atan2(u[1], u[0]);
    *ierr = 0;
    *rlat = atan(d1 * (float)1.0067391845079681 / sqrt((float)1. - d1 * d1));
    *rlon = atan2(u[1], u[0]);
    *ierr = 0;
    return 0;
} /* lpointgvarnv2_ */

/* Subroutine */ int gpointgvarnv2_(instr, flip_flg__, rlat, rlon, alf, gam, 
	ierr)
integer *instr, *flip_flg__;
doublereal *rlat, *rlon, *alf, *gam;
integer *ierr;
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(), sqrt(), cos(), atan(), tan();

    /* Local variables */
    static doublereal doff, sing, slat, f[3], u[3], alpha1, w1, w2, ff, ft[3];

    sing = sin(*rlat);
    w1 = radcomgvarnv2_1.aebe4c * sing * sing;
    ff = (doublereal) (*flip_flg__);
    if (*instr == 2) {
	ff = -ff;
    }
    doff = instcogvarnv2_1.scnmax[*instr - 1] - instcogvarnv2_1.ewnom[*instr 
	    - 1];
    slat = ((w1 * (float).375 - (float).5) * w1 + (float)1.) * sing / 
	    radcomgvarnv2_1.aebe2c;
    w2 = slat * slat;
    w1 = radcomgvarnv2_1.aebe3c * w2;
    w1 = (w1 * (float).375 - (float).5) * w1 + (float)1.;
    u[2] = slat * w1;
    w2 = w1 * sqrt((float)1. - w2);
    u[0] = w2 * cos(*rlon);
    u[1] = w2 * sin(*rlon);
    f[0] = u[0] - elcommgvarnv2_1.xs[0];
    f[1] = u[1] - elcommgvarnv2_1.xs[1];
    f[2] = u[2] - elcommgvarnv2_1.xs[2];
    w2 = u[0] * (real) f[0] + u[1] * (real) f[1] + u[2] * (real) f[2] * 
	    radcomgvarnv2_1.aebe2c;
    if (w2 > (float)0.) {
	*ierr = 1;
	*alf = (float)99999.;
	*gam = (float)99999.;
	return 0;
    }
    ft[0] = elcommgvarnv2_1.bt[0] * f[0] + elcommgvarnv2_1.bt[1] * f[1] + 
	    elcommgvarnv2_1.bt[2] * f[2];
    ft[1] = elcommgvarnv2_1.bt[3] * f[0] + elcommgvarnv2_1.bt[4] * f[1] + 
	    elcommgvarnv2_1.bt[5] * f[2];
    ft[2] = elcommgvarnv2_1.bt[6] * f[0] + elcommgvarnv2_1.bt[7] * f[1] + 
	    elcommgvarnv2_1.bt[8] * f[2];
/* Computing 2nd power */
    d__1 = ft[1];
/* Computing 2nd power */
    d__2 = ft[2];
    *gam = atan(ft[0] / sqrt(d__1 * d__1 + d__2 * d__2));
    *alf = -atan(ft[1] / ft[2]);
    w1 = sin(*alf);
    w2 = cos(*gam);
    alpha1 = *alf + elcommgvarnv2_1.rma * ((float)1. - cos(*alf) / w2) + 
	    elcommgvarnv2_1.pma * w1 * (ff / w2 + tan(*gam));
    *gam -= ff * elcommgvarnv2_1.rma * w1;
    *alf = alpha1 + alpha1 * *gam * doff;
    *gam -= alpha1 * (float).5 * alpha1 * doff;
    *ierr = 0;
    return 0;
} /* gpointgvarnv2_ */

/* Subroutine */ int gvranggvarnv2_(jday, jtime, xlat, xlon, gha, dec, satang,
	 sunang, relang)
integer *jday, *jtime;
real *xlat, *xlon, *gha, *dec, *satang, *sunang, *relang;
{
    /* Initialized data */

    static integer iday = 0;
    static real r__ = (float)6371.221;

    /* System generated locals */
    real r__1, r__2, r__3;

    /* Builtin functions */
    double sqrt(), sin(), cos(), acos(), atan2();

    /* Local variables */
    static real clat, sndc, clon, slat, snlg, xvec, yvec, zvec, ylat, slon, 
	    xsam, ysam, xsat, ysat, zsat, ylon, zsam, rdpdg;
    extern doublereal ftime_();
    static real xfact;
    static integer inorb;
    static real x1, y1, z1, x2, y2, x3, y3, z3, cosdec, height, us;
    extern doublereal geolat_();
    static real vs, ws, pictim, xc1, yc1, zc1, xc2, yc2, zc2, xan1, xan2, 
	    yan1, yan2, xan3, yan3;

    rdpdg = (float).017453292519943295;
    if (iday == *jday) {
	goto L1;
    }
    iday = *jday;
    inorb = 0;
L1:
    pictim = ftime_(jtime);
    xsat = elcommgvarnv2_1.xs[0] * 6378.137;
    ysat = elcommgvarnv2_1.xs[1] * 6378.137;
    zsat = elcommgvarnv2_1.xs[2] * 6378.137;
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
    snlg = -pictim * 3.141592653589793 / (float)12. - rdpdg * *gha;
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
} /* gvranggvarnv2_ */

