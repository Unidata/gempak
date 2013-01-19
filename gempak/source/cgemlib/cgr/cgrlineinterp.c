#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"


/*=====================================================================*/

/*
 * Private functions
 */
void lineinterp_insert ( int *np, float *x, float *y, float dist );

/*
 * Public functions
 */
void cgr_lineinterp ( int *npin0, float *xin0, float *yin0, 
	              int *npin1, float *xin1, float *yin1, float *pct,
	              int *maxnpo, int *npo, float *xo, float *yo, 
		      int *iret )
/************************************************************************
 * cgr_lineinterp							*
 *									*
 * This function takes two open lines and calculates an interpolated	*
 * open line using an interpolation percentage 'pct' (e.g.,0.5 for 50%).*
 * Cartesian coordinates are assumed.					*
 *									*
 * cgr_lineinterp ( npin0, xin0, yin0, npin1, xin1, yin1, pct,		*
 * 		    nmp, xmp0, ymp0, xmp1, ymp1, maxnpo,		*
 * 	            npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 *	*npin0		int	Number of points in line1		*
 *	*xin0		float	X coordinates of line 1			*
 *	*yin0		float	Y coordinates of line 1			*
 *	*npin1		int	Number of points in line2		*
 *	*xin1		float	X coordinates of line 2			*
 *	*yin1		float	Y coordinates of line 2			*
 *	*pct		float	Interpolation percentage           	*
 *	*maxnpo		int	Maximum number of points in output poly	*
 *									*
 * Output parameters:							*
 *	*npo		int	Number of points in output line		*
 *	*xo		float	X coordinates of output line		*
 *	*yo		float	Y coordinates of output line		*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/05						*
 ***********************************************************************/
{
int	ii;
int	npin0_t, npin1_t, np_max;
float	*xin0_t, *yin0_t, *xin1_t, *yin1_t;
float	*xmap0, *ymap0, *xmap1, *ymap1;
float	dx, dy, ttl0, ttl1, tx0, tx1, pct0, pct1;
/*---------------------------------------------------------------------*/

    *iret = 0;

    np_max = *npin0 + *npin1;
    G_MALLOC ( xin0_t, float, np_max, "Error allocating xin0_t" );
    G_MALLOC ( yin0_t, float, np_max, "Error allocating yin0_t" );
    G_MALLOC ( xin1_t, float, np_max, "Error allocating xin1_t" );
    G_MALLOC ( yin1_t, float, np_max, "Error allocating yin1_t" );

    G_MALLOC ( xmap0, float, np_max, "Error allocating xmap0" );
    G_MALLOC ( ymap0, float, np_max, "Error allocating ymap0" );
    G_MALLOC ( xmap1, float, np_max, "Error allocating xmap1" );
    G_MALLOC ( ymap1, float, np_max, "Error allocating ymap1" );

    /*
     * Make sure both lines have same number of points.
     */
    npin0_t = *npin0;
    memcpy ( xin0_t, xin0, *npin0*sizeof(float) );
    memcpy ( yin0_t, yin0, *npin0*sizeof(float) );
    npin1_t = *npin1;
    memcpy ( xin1_t, xin1, *npin1*sizeof(float) );
    memcpy ( yin1_t, yin1, *npin1*sizeof(float) );

    if ( *npin0 != *npin1 )  {
	ttl0 = ttl1 = 0.0F;
	for ( ii = 1; ii < *npin0; ii++ )  {
	    dx = xin0[ii]-xin0[ii-1];
	    dy = yin0[ii]-yin0[ii-1];
	    ttl0 += (float)(sqrt((double)(dx*dx+dy*dy)));
	}
	for ( ii = 1; ii < *npin1; ii++ )  {
	    dx = xin1[ii]-xin1[ii-1];
	    dy = yin1[ii]-yin1[ii-1];
	    ttl1 += (float)(sqrt((double)(dx*dx+dy*dy)));
	}
	tx0 = 0.0F;
	for ( ii = 1; ii < *npin0-1; ii++ )  {
	    dx = xin0[ii]-xin0[ii-1];
	    dy = yin0[ii]-yin0[ii-1];
	    tx0 += (float)(sqrt((double)(dx*dx+dy*dy)));
	    pct0 = tx0 / ttl0;
	    lineinterp_insert ( &npin1_t, xin1_t, yin1_t, pct0*ttl1 );
	}
	tx1 = 0.0F;
	for ( ii = 1; ii < *npin1-1; ii++ )  {
	    dx = xin1[ii]-xin1[ii-1];
	    dy = yin1[ii]-yin1[ii-1];
	    tx1 += (float)(sqrt((double)(dx*dx+dy*dy)));
	    pct1 = tx1 / ttl1;
	    lineinterp_insert ( &npin0_t, xin0_t, yin0_t, pct1*ttl0 );
	}
    }

    if ( npin0_t > *maxnpo )  {
	*iret = -1;
	*npo = 0;
	return;
    }

    /*
     * npin0_t and npin1_t must be equal...
     */
    *npo = npin0_t;
    for ( ii = 0; ii < *npo; ii++ )  {
	xo[ii] = xin0_t[ii] + (*pct)*(xin1_t[ii]-xin0_t[ii]);
	yo[ii] = yin0_t[ii] + (*pct)*(yin1_t[ii]-yin0_t[ii]);
    }

    G_FREE ( xin0_t, float );
    G_FREE ( yin0_t, float );
    G_FREE ( xin1_t, float );
    G_FREE ( yin1_t, float );

    G_FREE ( xmap0, float );
    G_FREE ( ymap0, float );
    G_FREE ( xmap1, float );
    G_FREE ( ymap1, float );

}

/*=====================================================================*/

void lineinterp_insert ( int *np, float *x, float *y, float dist )
/************************************************************************
 * lineinterp_insert							*
 *									*
 * This function inserts a point on a line at a distance along the line.*
 * It is assumed that the arrays are big enough to hold one more point.	*
 * It is furthur assumed that the distance is greater than zero, and	*
 * is less than the total length of the line.				*
 *									*
 * lineinterp_insert ( np, x, y, pct, ttl )				*
 *									*
 * Input parameters:							*
 *	*np		int	Number of points in line		*
 *	*x		float	X coordinates of line 			*
 *	*y		float	Y coordinates of line 			*
 *	dist		float	Distance				*
 *									*
 * Output parameters:							*
 *	*np		int	Number of points in line		*
 *	*x		float	X coordinates of line 			*
 *	*y		float	Y coordinates of line 			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/05						*
 ***********************************************************************/
{
int	ii, jj;
float	tx, tx_last, dt, dtd, pct, dx, dy, x_new, y_new;
/*---------------------------------------------------------------------*/

    tx = tx_last = 0.0F;
    for ( ii = 1; ii < *np; ii++ )  {
	dx = x[ii]-x[ii-1];
	dy = y[ii]-y[ii-1];
	tx += (float)(sqrt((double)(dx*dx+dy*dy)));
	if ( tx >= dist )  {
	    dt = tx - tx_last;
	    dtd = dist - tx_last;
	    pct = dtd / dt;
	    x_new = x[ii-1] + pct*(x[ii]-x[ii-1]);
	    y_new = y[ii-1] + pct*(y[ii]-y[ii-1]);
	    for ( jj = *np; jj > ii; jj-- )  {
		x[jj] = x[jj-1];
		y[jj] = y[jj-1];
	    }
	    x[ii] = x_new;
	    y[ii] = y_new;
	    *np += 1;
	    return;
	}
	tx_last = tx;
    }
}

/*=====================================================================*/
