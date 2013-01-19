#include "geminc.h"
#include "gemprm.h"

void cgr_segint ( char *sys1, float *xin1, float *yin1, char *sys2, 
			float *xin2, float *yin2, char *sys3, float *xint, 
			float *yint, int *intrsct, int *iret )
/************************************************************************
 * cgr_segint								*
 *									*
 * This function accepts two line segments and determines if they	*
 * intersect one another.  Note that if two line segments are extended	*
 * as lines, they will always intersect (unless they are parallel, ie.	*
 * their slopes are equal).  This intersecting point is returned	*
 * regardless of whether it falls on the segments themselves.  If the	*
 * segments are parallel, the intersecting point is (RMISSD,RMISSD).	*
 *									*
 * cgr_segint ( sys1, xin1, yin1, sys2, xin2, yin2, sys3, xint, yint,	*
 *		intrsct, iret )						*
 *									*
 * Input parameters:							*
 *	*sys1	char	Coordinate system for segment #1		*
 *	*xin1	float	X-coordinate of endpoints for segment #1	*
 *	*yin1	float	Y-coordinate of endpoints for segment #1	*
 *	*sys2	char	Coordinate system for segment #2		*
 *	*xin2	float	X-coordinate of endpoints for segment #2	*
 *	*yin2	float	Y-coordinate of endpoints for segment #2	*
 *	*sys3	char	Coordinate system for intersecting point	*
 *									*
 * Output parameters:							*
 *	*xint	float	X-coordinate of intersecting point		*
 *	*yint	float	Y-coordinate of intersecting point		*
 *	*intrsct int	Result: 					*
 *			0-FALSE (the segments do not intersect),	*
 *			1-TRUE (the segments intersect)			*
 *	*iret	 int	Return code					*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 7/97	Created					*
 * D.W.Plummer/NCEP	11/97	Added sys1,sys2 to calling sequence	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * T. Piper/GSC		10/98	Prolog update				*
 * D.W.Plummer/NCEP	 9/99	Chg calling seq to incl intrsctng point	*
 * A. Hardy/GSC         11/00   renamed coord. system output device 	*
 * D.W.Plummer/NCEP	11/02	Add explicit chk for horizontal segs	*
 ***********************************************************************/
{
int 	n, ier;
float	x, y, m1, b1, m2, b2;
float	x1[2], y1[2], x2[2], y2[2];
/*---------------------------------------------------------------------*/

    *iret = 0;
    *intrsct = 0;
    *xint = RMISSD;
    *yint = RMISSD;

    x = RMISSD;
    y = RMISSD;

    /*
     *  First put input points into normalized ("N") coordinates.
     */

    n = 2;
    gtrans( sys1, sys_N, &n, xin1, yin1, x1, y1, &ier, 
		strlen(sys1), strlen(sys_N) );
    gtrans( sys2, sys_N, &n, xin2, yin2, x2, y2, &ier, 
		strlen(sys2), strlen(sys_N) );

    /*
     *  Check for vertical first segment and compute (x,y) intersect.
     */

    if ( G_DIFF(x1[0], x1[1]) ) {

	x = x1[0];
	if ( G_DIFF(x2[0], x2[1]) )  return;
	m2 = (y2[1]-y2[0]) / (x2[1]-x2[0]);
	b2 = y2[0] - m2 * x2[0];
	y = m2 * x + b2;

    }

    /*
     *  Check for vertical second segment and compute (x,y) intersect.
     */

    else if ( G_DIFF(x2[0], x2[1]) ) {

	x = x2[0];
	if ( G_DIFF(x1[0], x1[1]) )  return;
	m1 = (y1[1]-y1[0]) / (x1[1]-x1[0]);
	b1 = y1[0] - m1 * x1[0];
	y = m1 * x + b1;

    }

    /*
     *  Finally compute (x,y) intersect for all other cases.
     */

    else {

    	m1 = (y1[1]-y1[0]) / (x1[1]-x1[0]);
    	b1 = y1[0] - m1 * x1[0];

    	m2 = (y2[1]-y2[0]) / (x2[1]-x2[0]);
    	b2 = y2[0] - m2 * x2[0];

	if ( G_DIFF(m1, m2) )  {
	    x = RMISSD;
	    y = RMISSD;
	}
	else  {
	    if ( G_DIFF(m1, 0.0F) )  {
	        x = ( b2 - y1[0] ) / ( - m2 );
	        y = y1[0];
	    }
	    else if ( G_DIFF(m2, 0.0F) )  {
	        x = ( y2[0] - b1 ) / ( m1 );
	        y = y2[0];
	    }
	    else  {
	        x = ( b2 - b1 ) / ( m1 - m2 );
	        y = m1 * x + b1;
	    }
	}

    }

    /*
     *  Check if intersecting point is within each segment's bounds.
     */

    if ( ERMISS(x) || ERMISS(y) )  return;

    n = 1;
    gtrans( sys_N, sys3, &n, &x, &y, xint, yint, &ier, 
		strlen(sys_N), strlen(sys3) );

    if ( x < G_MIN(x1[0],x1[1]) || x > G_MAX(x1[0],x1[1]) )  return;
    if ( x < G_MIN(x2[0],x2[1]) || x > G_MAX(x2[0],x2[1]) )  return;
    if ( y < G_MIN(y1[0],y1[1]) || y > G_MAX(y1[0],y1[1]) )  return;
    if ( y < G_MIN(y2[0],y2[1]) || y > G_MAX(y2[0],y2[1]) )  return;

    *intrsct = 1;

    return;

}
