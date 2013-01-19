#include "geminc.h"
#include "gemprm.h"

typedef struct point { double x, y, t; } Point;

#define T(p)	((p)->t)
#define X(p)	((p)->x)
#define Y(p)	((p)->y)

static float	*xinpts;
static float	*yinpts;
static float	*xoutpts;
static float	*youtpts;
static float	tol;
static int	ninpts;
static int	noutpts;

static void aspc_gamma ( Point *p );
static int  aspc_flat ( Point *p, Point *q , Point *r );
static void aspc_line ( Point *p, Point *q );
static void aspc_sample ( Point *p, Point *q );
static void aspc_aspc ( int a, int b );
/************************************************************************
 * cvrduc.c								*
 *									*
 * CONTENTS:								*
 *									*
 * cv_rduc								*
 * aspc_aspc								*
 * aspc_flat								*
 * aspc_gamma								*
 * aspc_line								*
 * aspc_sample								*
 ***********************************************************************/

void cv_rduc ( int *npts, float xpts[], float ypts[], float *filt,
               int *nout, float xout[], float yout[], int *iret )
/************************************************************************
 * CV_RDUC                                                              *
 *                                                                      *
 * This routine reduces the number of points on a line while            *
 * maintaining the shape of the line.  For smaller values of FILT, the  *
 * output line will more closely match the original line.  For larger   *
 * values more points will be removed.                                  *
 *									*
 * The values of the filter will be checked by GSRDUC. The user input	*
 * is valid for values between 0 and 1, inclusive. However, the values	*
 * for filter may be larger than 1 if they come from the individual	*
 * device drivers.							*
 *                                                                      *
 * "Adaptive Sampling of Parameteric Curves", L. H. de Figueiredo,      *
 * Graphics Gems V, Academic Press, 1995, pp 173-178                    *
 *                                                                      *
 * CV_RDUC ( NPTS, XPTS, YPTS, FILT, NOUT, XOUT, YOUT, IRET )           *
 *                                                                      *
 * Input parameters:                                                    *
 *      *npts           int             Number of input points          *
 *      xpts[]	 	float           X input coordinates             *
 *      ypts[]		float           Y input coordinates             *
 *      *filt           float           Filter factor                   *
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout           int             Number of output points         *
 *      xout[]          float           X output coordinates            *
 *      yout[]          float           Y output coordinates            *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 7/02						*
 * S. Jacobs/NCEP	12/02	Added checks on value of tolerance	*
 * S. Jacobs/NCEP	12/02	Removed checks on value of tolerance	*
 ***********************************************************************/
{
    int		j, inc,	a, b;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Init some module scope variables.
     */
    tol = *filt * 0.05;

    xinpts = xpts;
    yinpts = ypts;
    ninpts = *npts;
    xoutpts = xout;
    youtpts = yout;
    noutpts = 0;

    /*
     * Process 10% of the input points at a time. This will allow the use
     * of the mid-point as the start of the computation and will avoid 
     * the subtle form of sampling error for sinusoid curve.
     */
    inc = ( ninpts / 10 ) - 1;
    if ( inc < 1 )  inc = 1;

    for ( j = 0; j < ninpts; j += inc ) {
    	a = j;
    	b = (j + inc) > (ninpts - 1) ? (ninpts - 1) : (j + inc);
    	aspc_aspc ( a, b );
    }

    *nout = noutpts;

}

static void aspc_gamma ( Point *p )
/************************************************************************
 * aspc_gamma								*
 *									*
 * This routine caculates the point value from the point index		*
 *									*
 * aspc_gamma ( p )							*
 *									*
 * Input parameters:							*
 *	*p		Point		A point				*
 * Output parameters:							*
 *	None								*
 * Log:                                                                 *
 * R. Tian/SAIC		 7/02						*
 * S. Jacobs/NCEP	12/02	Changed computation of X and Y for pts	*
 * S. Jacobs/NCEP	12/02	Added check for crossing the dateline	*
 * S. Jacobs/NCEP	12/02	Added check on the array index value	*
 ***********************************************************************/
{
    int		index;
    float	fr, tlon;
/*---------------------------------------------------------------------*/

    index = (int) T(p);
    fr = T(p) - index;

    if  ( G_DIFFT(fr, 0.0F, GDIFFD) )  {
    	fr = 1.0;
    }
    else {
    	index += 1;
    }

    if  ( G_DIFFT(T(p), 0.0F, GDIFFD) )  {
	X(p) = xinpts[index];
	Y(p) = yinpts[index];
    }
    else {

	/*
	 * Check for crossing the international dateline.
	 */
	if  ( ( -180.0F <= yinpts[index-1] &&
			  yinpts[index-1] <= -150.0F ) &&
	      (  150.0F <= yinpts[index]   &&
	      		  yinpts[index]   <=  180.0F ) )  {
	    tlon = yinpts[index-1] + 360.0F;
	    X(p) = xinpts[index-1] + (xinpts[index] - xinpts[index-1]) * fr;
	    Y(p) = tlon + (yinpts[index] - tlon) * fr;
	}
	else if  ( ( -180.0F <= yinpts[index]   &&
			       yinpts[index]   <= -150.0F ) &&
	           (  150.0F <= yinpts[index-1] &&
		   	       yinpts[index-1] <=  180.0F ) )  {
	    tlon = yinpts[index] + 360.0F;
	    X(p) = xinpts[index-1] + (xinpts[index] - xinpts[index-1]) * fr;
	    Y(p) = yinpts[index-1] + (tlon - yinpts[index-1]) * fr;
	}
	else {
	    X(p) = xinpts[index-1] + (xinpts[index] - xinpts[index-1]) * fr;
	    Y(p) = yinpts[index-1] + (yinpts[index] - yinpts[index-1]) * fr;
	}

    }

}

static int aspc_flat ( Point *p, Point *q , Point *r )
/************************************************************************
 * aspc_flat								*
 *									*
 * This routine tests flatness between two points			*
 *									*
 * static int aspc_flat ( p, q, r )					*
 *									*
 * Input parameters:							*
 *	*p		Point		Starting point			*
 *	*q		Point		Ending point			*
 *	*r		Point		Middle point			*
 * Output parameters:							*
 *	None								*
 * Return parameters:							*
 *	aspc_flat	static int	Flatness			*
 * Log:                                                                 *
 * R. Tian/SAIC		 7/02						*
 * S. Jacobs/NCEP	12/02	Separated declaration and computation	*
 ***********************************************************************/
{
    float	xp, yp, xq, yq, z;
/*---------------------------------------------------------------------*/

    xp = X(p) - X(r);
    yp = Y(p) - Y(r);

    xq = X(q) - X(r);
    yq = Y(q) - Y(r);

    z = xp * yq - xq * yp;

    return ( (z * z) < tol );
}

static void aspc_line ( Point *p, Point *q )
/************************************************************************
 * aspc_line								*
 *									*
 * This routine places the sampled points into the output array		*
 *									*
 * aspc_line ( p, q )							*
 *									*
 * Input parameters:							*
 *	*p		Point		Starting point			*
 *	*q		Point		Ending point			*
 * Output parameters:							*
 *	None								*
 * Log:                                                                 *
 * R. Tian/SAIC		 7/02                                           *
 * S. Jacobs/NCEP	12/02	Only add the point if it is different	*
 * S. Jacobs/NCEP	12/02	Fixed logic on point check		*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    /*
     * The first point is always in the output.
     */
    if  ( noutpts == 0 )  {
    	xoutpts[noutpts] = X(p);
    	youtpts[noutpts] = Y(p);
	noutpts++;
    } 

    if  ( !G_DIFF(X(q), xoutpts[noutpts-1]) ||
    	  !G_DIFF(Y(q), youtpts[noutpts-1]) )  {
	xoutpts[noutpts] = X(q);
	youtpts[noutpts] = Y(q);
	noutpts++;
    }
}

static void aspc_sample ( Point *p, Point *q )
/************************************************************************
 * aspc_sample								*
 *									*
 * An routine modified from "Adaptive Sampling of Parameteric Curves"	*
 *									*
 * aspc_sample ( p, q )							*
 *									*
 * Input parameters:							*
 *	*p		Point		Starting point of sampling	*
 *	*q		Point		Ending point of sampling	*
 * Output parameters:							*
 *	None								*
 * Log:                                                                 *
 * R. Tian/SAIC		 7/02                                           *
 * S. Jacobs/NCEP	12/02	Changed to use a random number for the	*
 *				"midpoint" of the two endpoints		*
 * S. Jacobs/NCEP	12/02	Moved the random number seed function	*
 ***********************************************************************/
{
    Point	rr, *r = &rr;
    double	t;
/*---------------------------------------------------------------------*/

    t = 0.45 + 0.1 * ( rand() / (double) RAND_MAX );
    T(r) = ( T(p) + t * ( T(q) - T(p) ) );

    aspc_gamma ( r );

    if  ( aspc_flat ( p, q, r ) )  {
    	aspc_line ( p, q );
    }
    else {
    	aspc_sample ( p, r );
    	aspc_sample ( r, q );
    }

}

static void aspc_aspc ( int a, int b )
/************************************************************************
 * aspc_aspc								*
 *									*
 * An routine modified from "Adaptive Sampling of Parameteric Curves"	*
 *									*
 * aspc_aspc ( a, b )							*
 *									*
 * Input parameters:							*
 *	a		int		Starting point index 		*
 *	b		int		Ending point index		*
 * Output parameters:							*
 *	None								*
 * Log:                                                                 *
 * R. Tian/SAIC		7/02                                            *
 * S. Jacobs/NCEP	12/02	Moved the random number seed function	*
 ***********************************************************************/
{
    Point	pp, *p = &pp;
    Point	qq, *q = &qq;
/*---------------------------------------------------------------------*/

    srand ( time(NULL) );

    T(p) = a; aspc_gamma ( p );
    T(q) = b; aspc_gamma ( q );

    aspc_sample ( p, q );
}
