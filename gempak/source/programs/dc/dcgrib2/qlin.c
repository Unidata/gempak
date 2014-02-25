#include <stdio.h>
#include <stdlib.h>

#include <dccmn.h>
/*
 * Check return from malloc() and just exit (with a message) if we're out
 * of memory.
 */
void *emalloc (size)
    size_t size;
{
    int  ier;
    char errstr[DCMXLN];

    void   *p = (void *) malloc (size);
    if (p == 0) {
        sprintf(errstr, "malloc: out of memory\0");
	dc_wclg ( 0, "DCGRIB2", 0, errstr, &ier);
        exit (1);
    }
    return p;
}

/*
 * to regrid data from one equally-spaced grid to a different equally-spaced
 * grid.
 */
static void
linear(y, n, v, m, c)
    float *y;                   /* values of a function defined on an
                                   equally-spaced domain, y[0], ..., y[n] */
    int n;                      /* number of input y values */
    float *v;                   /* output values regridded onto new
                                   equally-spaced grid over same domain */
    int m;                      /* number of output v values */
    double *c;                  /* m precomputed interpolation coefficients:
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

void qlin(int nrows, int ix[], float *idat, int ni, int nj, float *odat)
/*************************************************************************
*   int nrows;                  number of rows in input			*
*   int ix[];                   row i starts at idat[ix[i]], and	*
*                                 ix[nrows] is 1 after last elem of idat* 
*   float *idat;                input quasi-regular data 		*
*   int ni;                     constant length of each output row 	*
*   int nj;                     number of output rows 			*
*   float *odat;                where to put ni*nj outputs, already	*
*                                   allocated 				*
*************************************************************************/
{
    int i, j;
    float *outp;
    static int NI=0;
    static double *c=NULL;
    static float *row2=NULL;

    /*
     ** In general, ni is not large, so don't continuously reallocate
     */
    if ( NI == 0 ) {
        c = (double *)emalloc(ni * sizeof(double));
        row2 = (float *)emalloc(ni * sizeof(float));
	NI = ni;
    }

    if ( ni > NI ) {
	c = (double *)realloc(c, ni * sizeof(double));
	row2 = (float *)realloc(row2, ni * sizeof(float));
	NI = ni;
    }

                                /* precompute interpolation coefficients */
    for (i=0; i < ni; i++)
        c[i] = (double) (ni - i - 1) / (ni - 1);

    outp = odat;
    for (j=0; j < nj; j++) {
        int inrow;              /* input row to use */
                                /* Does jth output row correspond to one of
                                   input rows, or is it between two?  */
        inrow = j*(nrows-1)/(nj-1);
        if (inrow * (nj-1) == j*(nrows-1)) { /* one row */
                                /* interpolate inrow row to ni points */
            linear(&idat[ix[inrow]],/* input row */
                   ix[inrow+1] - ix[inrow], /* number of points in row */
                   outp,        /* where to put new output row */
                   ni,          /* number of output values to compute */
                   c);          /* precomputed interpolation coefficients */
            outp += ni;
        } else {                /* between two rows */
                                /* interpolate rows inrow, inrow+1 to ni
                                   points  */
            linear(&idat[ix[inrow]],/* first input row */
                   ix[inrow+1] - ix[inrow], /* # of points in row */
                   outp,        /* where to put new output row */
                   ni,          /* number of output values to compute */
                   c);          /* precomputed interpolation coefficients */
            linear(&idat[ix[inrow+1]],/* second input row */
                   ix[inrow+2] - ix[inrow+1], /* # of points in row */
                   row2,        /* where to put new output row */
                   ni,          /* number of output values to compute */
                   c);          /* precomputed interpolation coefficients */
            for (i=0; i < ni; i++) {
                double c1 = 1.0 - j*(nrows - 1.0)/(nj - 1.0);
                double c2 = 1.0 - c1;
                *outp = c1 * *outp + c2*row2[i];
                outp++;
            }
        }
    }
}
