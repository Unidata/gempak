#include "geminc.h"
#include "gemprm.h"

#ifndef FLT_MAX
#define	FLT_MAX	1E+37
#endif

void cgr_segdist ( int *np, float *xx, float *yy, float *fx, float *fy, 
			float *distance, int *nearest_vrt, int *next_vrt, 
			float *nx, float *ny, int *iret )
/************************************************************************
 * cgr_segdist								*
 *									*
 * This function determines the nearest and next vertices of a		*
 * multipoint line to a fixed point, the closest point (on the line	*
 * segment defined by those two vertices) to the fixed point, and the	*
 * distance between the fixed point and the closest point.		*
 * The "next vertex" is simply the vertex following the nearest vertex	*
 * in the order of the points, not the next closest vertex to the fixed	*
 * point.								*
 *									*
 * cgr_segdist ( np, xx, yy, fx, fy, distance, nearest_vrt, next_vrt,	*
 *                                         nx, ny, iret )		*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points in figure	*
 *	*xx		float		X coordinates of figure		*
 *	*yy		float		Y coordinates of figure		*
 *	*fx		float		X coordinate of fixed point	*
 *	*fy		float		Y coordinate of fixed point	*
 *									*
 * Output parameters:							*
 *	*distance	float		Distance to the point		*
 *	*nearest_vrt	int		Closest vertex number		*
 *	*next_vrt	int		Other end of nearest segment	*
 *	*nx		float		Nearest x coord on figure	*
 *	*ny		float		Nearest y coord on figure	*
 *	*iret		int		Status return			*
 *					0 = great, 1 = not a line	*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	02/98	copied cgr_dist				*
 * E. Safford/GSC	05/98	add G_NINT to handle rounding problem	*
 * E. Safford/GSC	07/98	add equal condition to horiz & vert	*
 * T. Piper/GSC		10/98	Prolog update				*
 * S. Law/GSC		03/99	clean up and commentary			*
 * W.D.Plummer/NCEP	12/02	make all inputs pointers		*
 * W.D.Plummer/NCEP	02/03	expand documenation in prologue		*
 ***********************************************************************/
{
    int		ii;
    float	qx, qy, curr_dist, d0, d1, m2, m1, b2, b1;
    float	xmin, xmax, ymin, ymax;
/*---------------------------------------------------------------------*/

    if (*np == 1) {
	*iret = 1;

	*nearest_vrt = *next_vrt = 0;
	*nx = xx[0];
	*ny = yy[0];

	*distance = (float) G_DIST (xx[0], yy[0], *fx, *fy);

	return;
    }

    *iret = 0;
    *distance = FLT_MAX;

    /*
     * Isolate which line segment is closest to desired point.
     */

    for (ii = 0; ii < *np-1; ii++ ) {

  	xmin = (float) G_MIN (xx[ii], xx[ii+1]);
	xmax = (float) G_MAX (xx[ii], xx[ii+1]);
	ymin = (float) G_MIN (yy[ii], yy[ii+1]);
	ymax = (float) G_MAX (yy[ii], yy[ii+1]);

	/*
	 * Must find the closest point on vertical and horiztonal
	 * seperately since the slope formula would cause a 
	 * divide by zero error
	 */

	/*
	 *  Vertical segments
	 */
	if (G_DIFF(xmin, xmax)) {
	    qx = xmin;
	    if (*fy < ymin)
	        qy = ymin;
	    else if (*fy > ymax)
		qy = ymax;
	    else
		qy = *fy;
	}

	/*
	 *  Horizontal segments
	 */
    	else if ( G_DIFF(ymin, ymax) ) {
	    qy = ymin;
	    if (*fx < xmin)
	        qx = xmin;
	    else if (*fx > xmax)
		qx = xmax;
	    else
		qx = *fx;
	}

	/*
	 *  All the rest
	 */
	else {
	    /*
	     * find slope and intercept for initial line
	     */
	    m1 = (yy[ii+1] - yy[ii]) / (xx[ii+1] - xx[ii]);
	    b1 = yy[ii] - (m1 * xx[ii]);

	    /*
	     * find slope and intercept for perpendicular
	     */
	    m2 = - 1.0F / m1;
	    b2 = *fy - (m2 * *fx);

	    /* 
	     * find the intersection of the two lines
	     * which would be the closest point
	     *
	     * formula for a line is y = mx + b
	     * y = (m1 * x) + b1  &&  y = (m2 * x) + b2
	     * (m1 * x) + b1 = (m2 * x) + b2
	     * (m1 * x) - (m2 * x) = (b2 - b1)
	     * x * (m1 - m2) = (b2 - b1)
	     * x = (b2 - b1) / (m1 - m2)
	     */
	    qx = (b2 - b1) / (m1 - m2);
	    qy = (m2 * qx) + b2;
	}

	/*
	 * find the distance
	 */
	if (xmin <= qx && qx <= xmax) {
	    curr_dist = (float) G_DIST (*fx, *fy, qx, qy);
	}
	else {
	    d0 = (float) G_DIST (*fx, *fy, xx[ii], yy[ii]);
	    d1 = (float) G_DIST (*fx, *fy, xx[ii+1], yy[ii+1]);
	    curr_dist = (d0 <= d1) ? d0 : d1;
	}

	if (curr_dist < *distance) {
	    *distance = curr_dist;

	    *nx = qx;
	    *ny = qy;	

	    /*
	     *  Figure which end of segment is closest to point.
	     */
	    d0 = (float) G_DIST (*fx, *fy, xx[ii], yy[ii]);
	    d1 = (float) G_DIST (*fx, *fy, xx[ii+1], yy[ii+1]);

	    if (d0 < d1) {
		*nearest_vrt = ii;
		*next_vrt    = ii + 1;
	    }
	    else {
		*nearest_vrt = ii + 1;
		*next_vrt    = ii;
	    }

	    if ((*nx < xmin) || (xmax < *nx)) {
		*nx = xx[*nearest_vrt];
		*ny = yy[*nearest_vrt];
	    } 
	}
    }
}
