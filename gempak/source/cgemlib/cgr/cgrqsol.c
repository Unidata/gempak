#include "geminc.h"
#include "gemprm.h"

void cgr_qsol ( int np, float *xx, float *yy, float fx, float fy, 
					int *lor, int *aob, int *iret )
/************************************************************************
 * cgr_qsol								*
 *									*
 * Given a point (fx, fy), this function determines whether the point	*
 * lies left or right and above or below a line (set of points xx,yy).	*
 * Left and above return -1; right or below return 1; on the line = 0.	*
 * For horizontal lines, use the convention above=left, below=right.	*
 * For vertical lines, use the convention left=above, right=below.	*
 * This function/algorithm has nothing to do with a point being "to the	*
 * right of a line" or "to the left of a line". Rather, this function	*
 * deals strictly with the relative positions of the two geometrical	*
 * elements (a point and a line/set of points).				*
 *									*
 * ALGORITHM:								*
 * 1) determine which line segment is closest to the point in question,	*
 * 2) calculate the slope and intercept of this closest segment, i.e.,	*
 *    line equation,							*
 * 3) use linear algebra to determine whether the point is above or	*
 *    below and left or right of this line.				*
 *									*
 * Note that the ordering of the line points is irrelevant.		*
 * Note that all points are assumed to be in the same cartesian 	*
 * coordinate system, not earth coordinates.				*
 *									*
 * cgr_qsol ( np, xx, yy, fx, fy, lor, aob, iret )			*
 *									*
 * Input parameters:							*
 *	np		int		Number of points in figure	*
 *	*xx		float		X coordinates of figure		*
 *	*yy		float		Y coordinates of figure		*
 *	fx		float		X coordinate of fixed point	*
 *	fy		float		Y coordinate of fixed point	*
 *									*
 * Output parameters:							*
 *	*lor		int		Left or right 			*
 *	*aob		int		Above or below			*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/01						*
 * W.D.Plummer/NCEP	12/02	chg call sequence to cgr_segdist	*
 * W.D.Plummer/NCEP	 2/03	expand prologue documentation		*
 ***********************************************************************/
{
    int		v1, v2, ier;
    float	d, xclo, yclo, slope, intrcpt, xv, yv;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (np == 1) {
        /*
         * Single point
         */
	if ( fx > xx[0] )
	    *lor = 1;
	else if ( fx < xx[0] )
	    *lor = -1;
	else
	    *lor = 0;
	if ( fy > yy[0] )
	    *aob = -1;
	else if ( fy < yy[0] )
	    *aob = 1;
	else
	    *aob = 0;
    }
    else {
        /*
         * Multiple point
         */
	cgr_segdist (&np, xx, yy, &fx, &fy, &d, &v1, &v2, &xclo, &yclo, &ier);

	if ( !G_DIFF(xx[v1], xx[v2]) && !G_DIFF(yy[v1], yy[v2]) )  {
	    /*
	     *  Normal case, solve for slope, intrcpt.
	     */
	    slope = (yy[v2]-yy[v1]) / (xx[v2]-xx[v1]);
	    intrcpt = yy[v1] - slope * xx[v1];

	    xv = ( fy - intrcpt ) / slope;
	    yv = slope * fx + intrcpt;

	    if ( fx < xv )
		*lor = -1;
	    else if ( fx > xv )
		*lor = 1;
	    else
		*lor = 0;
	    if ( fy < yv )
		*aob = 1;
	    else if ( fy > yv )
		*aob = -1;
	    else
		*aob = 0;
	}
	else if ( G_DIFF(yy[v1], yy[v2]) )  {
	    /*
	     *  Zero slope
	     */
	    if ( fy > yy[v1] )  {
		*lor = -1;
		*aob = -1;
	    }
	    else if ( fy < yy[v1] )  {
		*lor = 1;
		*aob = 1;
	    }
	    else  {
		*lor = 0;
		*aob = 0;
	    }
	}
	else if ( G_DIFF(xx[v1], xx[v2]) )  {
	    /*
	     *  Infinite slope
	     */
	    if ( fx > xx[v1] )  {
		*lor = 1;
		*aob = 1;
	    }
	    else if ( fx < xx[v1] )  {
		*lor = -1;
		*aob = -1;
	    }
	    else  {
		*lor = 0;
		*aob = 0;
	    }
	}
    }
}
