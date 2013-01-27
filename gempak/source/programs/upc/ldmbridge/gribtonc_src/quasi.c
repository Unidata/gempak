/*
 *   Copyright 1995, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: quasi.c,v 1.12 1996/01/10 16:22:06 russ Exp $ */

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>

#include "ulog.h"
#include "quasi.h"
#include "gdes.h"
#include "emalloc.h"

#ifdef __STDC__
static int getsubopt1(char **optionp, char * *tokens, char **valuep);
static void qlin(int nrows, int *ix, float *in, int ni, int nj, float *out);
static void qcub(int nrows, int *ix, float *in, int ni, int nj, float *out);
static void linear(float* y, int n, float* v, int m, double* c);
static void cspline(float* inpt, int n, double x1d, double xnd, float* y2d);
static void csplint(float* inpt, int n, float* y2d, double x, float* outpt);
#endif

/*
 * The SVID interface to getsubopt provides no way of figuring out which
 * part of the suboptions list wasn't matched.  This makes error messages
 * tricky...  The variable suboptarg is a pointer to the token
 * which didn't match.
 */
char *suboptarg;

char *tokens[] = {		/* tokens for use in comma-delimited qmeth
				   suboption, e.g. -q lin,dlat=1.25,dlon=2.5.
				   Add more at the end, but make sure list ends
				   with 0. */
#define LINEAR	0
    "lin",
#define CUBIC	1
    "cub",
#define DLAT	2
    "dlat",
#define DLON	3
    "dlon",
    0
};

/*
 * Parse qmeth string for specifying method used to expand quasi-regular grids.
 * Returns 0 if bad.
 */
quas *
qmeth_parse (qmeth)
    char *qmeth;		/* method for expanding quasi-linear "grids" */
{
    int errflg = 0;
    char *value;
    char *endptr;
    quas *ret = (quas *) emalloc(sizeof(quas));

    ret->meth = QUASI_METH_DEF;	/* set defaults */
    ret->dlat = 0.;
    ret->dlon = 0;
    
    while (*qmeth != '\0') {
	switch(getsubopt1(&qmeth, tokens, &value)) {
	case LINEAR:
	    ret->meth = QUASI_METH_LIN;
	    break;
	case CUBIC:
	    ret->meth = QUASI_METH_CUB;
	    break;
	case DLAT:
	    if (value == NULL) {
		uerror("dlat must have a value");
		errflg++;
	    }
	    ret->dlat = strtod(value, &endptr);
	    if (endptr == value) {
		uerror("dlat's value must be a number, '%s'", value);
		errflg++;
	    }
	    if (ret->dlat < 0. || ret->dlat > 90.) {
		uerror("dlat not in range [0.,90.]: %g", ret->dlat);
		errflg++;
	    }
	    break;
	case DLON:
	    if (value == NULL) {
		uerror("dlon must have a value");
		errflg++;
	    }
	    ret->dlon = strtod(value, &endptr);
	    if (endptr == value) {
		uerror("dlon's value must be a number, '%s'", value);
		errflg++;
	    }
	    if (ret->dlon < 0. || ret->dlon > 360.) {
		uerror("dlon not in range [0.,360.]: %g", ret->dlon);
		errflg++;
	    }
	    break;

	}
    }
    if (errflg) {
	free(ret);
	return 0;
    }

    return ret;
}


/*
 * to regrid data from one equally-spaced grid to a different equally-spaced
 * grid.
 */
static void
linear(y, n, v, m, c)
    float *y;			/* values of a function defined on an
				   equally-spaced domain, y[0], ..., y[n] */
    int n;			/* number of input y values */
    float *v;			/* output values regridded onto new
				   equally-spaced grid over same domain */
    int m;			/* number of output v values */
    double *c;			/* m precomputed interpolation coefficients:
				   for (j=0; j<m; j++)
				       c[j] = (double) (m - j -1) / (m - 1); */
{
    int i, j, k, skip;
    j = 0;
    k = 0;
    for (i=0; i<m; i++) {
	v[i] = c[j]*y[k] + c[m-j-1]*y[k+1];
	j += n-1;
	skip = (j-1)/(m-1);
	if (skip > 0) {
	    k +=  skip;
	    j = j - (m-1) * skip;
	}
    }
}


static void
qlin(nrows, ix, idat, ni, nj, odat)
    int nrows;			/* number of rows in input */
    int ix[];			/* row i starts at idat[ix[i]], and
				   ix[nrows] is 1 after last elem of idat */
    float *idat;		/* input quasi-regular data */
    int ni;			/* constant length of each output row */
    int nj;			/* number of output rows */
    float *odat;		/* where to put ni*nj outputs, already
				   allocated */
{
    int i, j;
    float *outp;
    double *c = (double *)emalloc(ni * sizeof(double));/* use scratch space? */
    float *row2 = (float *)emalloc(ni * sizeof(float));
    
				/* precompute interpolation coefficients */
    for (i=0; i < ni; i++)
	c[i] = (double) (ni - i - 1) / (ni - 1);

    outp = odat;
    for (j=0; j < nj; j++) {
	int inrow;		/* input row to use */
				/* Does jth output row correspond to one of
				   input rows, or is it between two?  */
	inrow = j*(nrows-1)/(nj-1);
	if (inrow * (nj-1) == j*(nrows-1)) { /* one row */
				/* interpolate inrow row to ni points */
	    linear(&idat[ix[inrow]],/* input row */
		   ix[inrow+1] - ix[inrow], /* number of points in row */
		   outp,	/* where to put new output row */
		   ni,		/* number of output values to compute */
		   c);		/* precomputed interpolation coefficients */
	    outp += ni;
	} else {		/* between two rows */
				/* interpolate rows inrow, inrow+1 to ni
				   points  */
	    linear(&idat[ix[inrow]],/* first input row */
		   ix[inrow+1] - ix[inrow], /* # of points in row */
		   outp,	/* where to put new output row */
		   ni,		/* number of output values to compute */
		   c);		/* precomputed interpolation coefficients */
	    linear(&idat[ix[inrow+1]],/* second input row */
		   ix[inrow+2] - ix[inrow+1], /* # of points in row */
		   row2,	/* where to put new output row */
		   ni,		/* number of output values to compute */
		   c);		/* precomputed interpolation coefficients */
	    for (i=0; i < ni; i++) {
		double c1 = 1.0 - j*(nrows - 1.0)/(nj - 1.0);
		double c2 = 1.0 - c1;
		*outp = c1 * *outp + c2*row2[i];
		outp++;
	    }
	}
    }
    free(row2);
    free(c);
}

static void
cspline(inpt, n, x1d, xnd, y2d)
    float *inpt; 					   /* input data row */
    int n; 				    /* number of points in input row */
    float x1d; 			  /* first derivative of the first end point */
    float xnd; 			    /* first derivative of the nth end point */
    float *y2d; 			    /* output row of 2nd derivatives */

{
    int i; 							      /* lcv */

    float p;
    float qn;
    float sig;
    float un;
    float *scratch = (float *)emalloc((n - 1) * sizeof(float));
                                                           /* scratch vector */

    if (x1d > 0.99e30) 				/* lower boundary is natural */
	y2d[0] = scratch[0] = 0;

    else { 			       /* calculate the lower boundary value */
       y2d[0] = 0.5;
       scratch[0] = 3.0 * ((inpt[1] - inpt[0]) / (1 - x1d));
    }

    for(i = 1;i < n - 1;i++) { 			       /* decomposition loop */
	sig = 0.5;
	p = sig * y2d[i-1] + 2.0;
	y2d[i] = (sig - 1.0) / p;
	scratch[i] = (inpt[i+1] - inpt[i]) - (inpt[i] - inpt[i-1]);
	scratch[i] = (6.0 * scratch[i] / 2.0 - sig * scratch[i-1]) / p;
    }
	

    if (xnd > 0.99e30) 				/* upper boundary is natural */
	qn = un = 0;

    else { 			       /* calculate the upper boundary value */
	qn = 0.5;
	un = 3.0 * (xnd - (inpt[n-1] - inpt[n-2]));
    }

    y2d[n-1] = (un -qn * scratch[n-2]) / (qn * y2d[n-2] + 1.0);

    for(i = n-2;i >= 0;i--)  			   /* back substitution loop */
	y2d[i] = y2d[i] * y2d[i+1] + scratch[i];

    free(scratch);
}


static void
csplint(inpt, n, y2d, x, outpt)
    float *inpt; 						/* input row */
    int n; 					   /* number of input values */
    float *y2d; 			  /* second derivatives of input row */
    float x; 						     /* output point */
    float *outpt; 		       /* where to put the interpolated data */
{
    int hi;
    int low;
    float a;
    float b;

    if (floor(x) == x) { 			      /* existing data point */
	*outpt = inpt[(int)x];
	return;
    }

/* set the input bracket */

    hi = (int)(ceil(x));
    low = (int)(floor(x));

    a = hi - x;
    b = x - low;

/* evalualte the polynomial */

    *outpt = a * inpt[low] + b * inpt[hi] + ((a * a * a - a) * y2d[low] +
	(b * b * b - b) * y2d[hi]) / 6.0;
}


static void
qcub(nrows, ix, idat, ni, nj, odat)
    int nrows;			/* number of rows in input */
    int ix[];			/* row i starts at idat[ix[i]], and
				   ix[nrows] is 1 after last elem of idat */
    float *idat;		/* input quasi-regular data */
    int ni;			/* constant length of each output row */
    int nj;			/* number of output rows */
    float *odat;		/* where to put ni*nj outputs, already
				   allocated */
{
    int i; 							      /* lcv */
    int j; 							      /* lcv */

    float x1_der = 1.0e30;		/* derivative of the first end point */
    float xn_der = 1.0e30;		  /* derivative of the nth end point */
    float *oput; 			      /* working data output pointer */

    float *second_d; 			  /* second derivatives of input row */



/* we assume that the output rows correspond to existing input rows */

    oput = odat;

    for (j = 0;j < nj;j++) {
	int inrow; 					 /* input row to use */
	int npoints; 		/* number of input points in the current row */

	inrow = j * (nrows - 1) / (nj - 1); 	       /* set the row number */
	npoints = ix[inrow+1] - ix[inrow];     /* set number of input points */

	second_d = (float *)emalloc(npoints * sizeof(double));
	                      /* calculated second derivative of a given row */

/* calculate the second derivatives of the input row */

	cspline(
	    &idat[ix[inrow]], 					/* input row */
	    npoints, 			   /*  number of points in input row */
	    x1_der, 			/* first derivative of first element */
	    xn_der, 			  /* first derivative of nth element */
	    second_d); 				 /* output second derivative */

/* interpolate the output row */

	for(i = 0;i < ni;i++) {
	    float mapped_i; 			  /* i mapped to input space */
	    mapped_i = (float)i / ((float)ni - 1) * ((float)npoints - 1);
		                          /* map output point to input space */
	    csplint( 				    /* interpolate the value */
		&idat[ix[inrow]], 				/* input row */
		npoints, 			   /* number of input points */
		second_d, 		    /* calculated second derivatives */
		mapped_i, 		       /* element to be interpolated */
		oput++); 	      /* where to put the interpolated value */
	}

	free(second_d);
    }
}


#define float_near(x,y)	((y) + 0.1*fabs((x)-(y)) == (y))

/*
 * Expands quasi-regular grid that is part of product_data to make a regular
 * lat-lon grid.  Returns 0 on error, 1 if succeeded.
 */
int
expand_quasi (quasp, pp)
    quas *quasp;
    product_data* pp;
{
    gdes *gdesp = pp->gd;
    int ni, nj, npts;
    float di, dj;
    float* data;

    if (! pp->has_gds) {
	uerror("GRIB %s, no GDS for quasi-regular grid, skipping",
	       pp->header);
	return 0;
    }
    switch(gdesp->type) {
    case GRID_LL:
    case GRID_RLL:
    case GRID_SLL:
    case GRID_SRLL:
	switch (gdesp->quasi) {
	    gdes_ll *g;
	    double lodiff;	/* longitude range */
	    double ladiff;	/* latitude range */
	case QUASI_ROWS:
	    g = &gdesp->grid.ll;
	    lodiff = g->lo2 - g->lo1;
	    while (lodiff <= 0.)
		lodiff += 360.;
	    if (quasp->dlon == 0.) { /* no dlon specified, so use di of
					max row size */
		ni = gdesp->maxlc;
		di = lodiff/(ni - 1);
	    } else {
		di = quasp->dlon; /* dlon specified on command line */
		ni = lodiff/di + 1;
	    }
	    if ( ! float_near((ni - 1) * di, lodiff) ) {
		uerror("GRIB %s: dlon (%g) must evenly divide longitude range (%g)",
		       pp->header, quasp->dlon, lodiff);
		return 0;
	    }

	    ladiff = g->la2 -g->la1;
	    
	    if (quasp->dlat == 0.) { /* no dlat specified, use dj in GDS */
		dj = g->dj;
		nj = g->nj;
	    } else {
		dj = quasp->dlat; /* dlat specified on command line */
		nj = ladiff/dj + 1;
	    }
	    if ( ! float_near((nj - 1) * dj, ladiff) ) {
		uerror("GRIB %s: dlat (%g) must evenly divide latitude range (%g)",
		       pp->header, quasp->dlat, ladiff);
		return 0;
	    }
	    npts = ni*nj;

	    data = (float *)emalloc(npts * sizeof(float));

	    /* interpolate from pp->data to data */

	    switch(quasp->meth) {
	    case QUASI_METH_DEF: /* fall through, default is linear for now */
	    case QUASI_METH_LIN:
		qlin(gdesp->nrows, gdesp->lc, pp->data, ni, nj, data);
		break;
	    case QUASI_METH_CUB:
		if ((gdesp->nrows - 1) % (nj - 1) == 0)
		    qcub(gdesp->nrows, gdesp->lc, pp->data, ni, nj, data);
		else
		    uerror("GRIB %s: output rows (%d) must evenly divide input rows (%d)",
			pp->header, nj, gdesp->nrows);
		break;
	    default:
		break;
	    }

	    g->ni = ni;
	    g->di = di;
	    g->nj = nj;
	    g->dj = dj;
	    free(pp->data);	/* free old data block */
	    pp->data = data;
	    gdesp->ncols = ni;
	    gdesp->nrows = nj;
	    gdesp->npts = npts;
	    gdesp->quasi = QUASI_RECT;
	    if (gdesp->lc) {
		free(gdesp->lc);
		gdesp->lc = 0;
	    }
	    pp->npts = npts;
	    break;
	case QUASI_COLS:
	    uerror("GRIB %s: can't handle quasi-regular, varying columns",
		   pp->header);
	    return 0;
	default:		/* should never happen */
	    uerror("Bad value for gdesp->type: %d",
		   gdesp->type);
	    return 0;
	}
	    break;
    case GRID_GAU:
    case GRID_RGAU:
    case GRID_SGAU:
    case GRID_SRGAU:
	uerror("GRIB %s: Can't handle quasi-regular Gaussian grids yet",
	       pp->header);
	return 0;
    default:
	uerror("In GRIB %s, wrong grid type (%d) for a quasi-regular grid",
	       pp->header, gdesp->type);
	return 0;
    }
    /*pp->grid = NONCATALOGED_GRID;*/
    pp->grid = QUASI_RECT_GRID;
    pp->cols = pp->gd->ncols;
    pp->npts = pp->gd->npts;
    
    if (pp->has_bms) {
	uerror("In GRIB %s, can't handle Bit Map Section with quasi-regular grid",
	       pp->header);
	return 0;
    }
    return 1;
}


/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

static int
getsubopt1(optionp, tokens, valuep)
	register char **optionp, **valuep;
	register char * *tokens;
{
	register int cnt;
	register char *p;

	suboptarg = *valuep = NULL;

	if (!optionp || !*optionp)
		return(-1);

	/* skip leading white-space, commas */
	for (p = *optionp; *p && (*p == ',' || *p == ' ' || *p == '\t'); ++p)
	    /* empty body */ ;

	if (!*p) {
		*optionp = p;
		return(-1);
	}

	/* save the start of the token, and skip the rest of the token. */
	for (suboptarg = p;
	    *++p && *p != ',' && *p != '=' && *p != ' ' && *p != '\t';)
	    /* empty body */ ;

	if (*p) {
		/*
		 * If there's an equals sign, set the value pointer, and
		 * skip over the value part of the token.  Terminate the
		 * token.
		 */
		if (*p == '=') {
			*p = '\0';
			for (*valuep = ++p;
			    *p && *p != ',' && *p != ' ' && *p != '\t'; ++p)
			    /* empty body */ ;
			if (*p) 
				*p++ = '\0';
		} else
			*p++ = '\0';
		/* Skip any whitespace or commas after this token. */
		for (; *p && (*p == ',' || *p == ' ' || *p == '\t'); ++p)
		    /* empty body */ ;
	}

	/* set optionp for next round. */
	*optionp = p;

	for (cnt = 0; *tokens; ++tokens, ++cnt)
		if (!strcmp(suboptarg, *tokens))
			return(cnt);
	return(-1);
}
