#include "geminc.h"
#include "gemprm.h"

void cgr_segintwn (  float *xin1, float *yin1, 
		     float *xin2, float *yin2,
		     float *xint, float *yint, 
		     int *intrsct, int *iret )
/************************************************************************
 * cgr_segintwn								*
 *									*
 * This function accepts two line segments and determines if they	*
 * intersect one another.  Note that if two line segments are extended	*
 * as lines, they will always intersect (unless they are parallel, ie.	*
 * their slopes are equal).  This intersecting point is returned	*
 * regardless of whether it falls on the segments themselves.  If the	*
 * segments are parallel, the intersecting point is (RMISSD,RMISSD).	*
 *									*
 * This is simply a copy of cgr_segint without the use of normalized    *
 * coordinates.  Output is in sys_M, inputs are assumed to be sys_M as  *
 * well.  If they are in any other coordinate system the results will   *
 * be undefined.							*
 *									*
 * cgr_segint ( xin1, yin1, xin2, yin2, xint, yint, intrsct, iret )	*
 *									*
 * Input parameters:							*
 *	*xin1	float	X-coordinate of endpoints for segment #1	*
 *	*yin1	float	Y-coordinate of endpoints for segment #1	*
 *	*xin2	float	X-coordinate of endpoints for segment #2	*
 *	*yin2	float	Y-coordinate of endpoints for segment #2	*
 *									*
 * Output parameters:							*
 *	*xint	float	X-coordinate of intersecting point		*
 *	*yint	float	Y-coordinate of intersecting point		*
 *	*intrsct int	Result: 					*
 *			0-FALSE (the segments do not intersect),	*
 *			1-TRUE (the segments intersect)			*
 *	*iret	 int	Return code					*
 **									*
 * Log:									*
 * E. Safford/SAIC	09/06	copied from cgr_segint 			*
 * E. Safford/SAIC	10/06	make internal variables double to       *
 *				 ensure same results on all platforms   *
 ***********************************************************************/
{
double	x, y, m1, b1, m2, b2;
double	x1[2], y1[2], x2[2], y2[2];
/*---------------------------------------------------------------------*/

    *iret = 0;
    *intrsct = 0;
    *xint = RMISSD;
    *yint = RMISSD;

    x = RMISSD;
    y = RMISSD;


    /*
     *  Make local copies of the inputs.
     */    
    x1[0] = xin1[0]; x1[1] = xin1[1];
    y1[0] = yin1[0]; y1[1] = yin1[1];
  
    x2[0] = xin2[0]; x2[1] = xin2[1];
    y2[0] = yin2[0]; y2[1] = yin2[1];


    /*
     *  Check for vertical first segment and compute (x,y) intersect.
     */

    if ( G_DIFF(x1[0], x1[1]) ) {

	x = x1[0];
	if ( G_DIFF(x2[0], x2[1]) ) return; 
	m2 = (y2[1]-y2[0]) / (x2[1]-x2[0]);
	b2 = y2[0] - m2 * x2[0];
	y = m2 * x + b2;

    }

    /*
     *  Check for vertical second segment and compute (x,y) intersect.
     */

    else if ( G_DIFF(x2[0], x2[1]) ) {

	x = x2[0];
	if ( G_DIFF(x1[0], x1[1]) ) return;
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

    if ( ERMISS(x) || ERMISS(y) ) return;

    *xint = x;
    *yint = y;

    if ( x < G_MIN(x1[0],x1[1]) || x > G_MAX(x1[0],x1[1]) )  return;
    if ( x < G_MIN(x2[0],x2[1]) || x > G_MAX(x2[0],x2[1]) )  return;
    if ( y < G_MIN(y1[0],y1[1]) || y > G_MAX(y1[0],y1[1]) )  return;
    if ( y < G_MIN(y2[0],y2[1]) || y > G_MAX(y2[0],y2[1]) )  return;

    *intrsct = 1;

    return;
}
