#include "geminc.h"
#include "gemprm.h"

void cgr_lindist ( float x1, float y1, float x2, float y2, float fx, 
		float fy, float *px, float *py, float *distance, int *iret )
/************************************************************************
 * cgr_lindist								*
 *									*
 * This function determines the distance from a fixed point to a line	*
 * defined by a pair of points.  It returns the distance to the line	*
 * as well as the closest point on the line to the fixed point.		*
 *									*
 * cgr_lindist ( x1, y1, x2, y2, fx, fy, px, py, distance, iret )	*
 *									*
 * Input parameters:							*
 *	x1		float		First x-coord point of line	*
 *	y1		float		First y-coord point of line	*
 *	x2		float		Second x-coord point of line	*
 *	y2		float		Second y-coord point of line	*
 *	fx		float		X coordinate of fixed point	*
 *	fy		float		Y coordinate of fixed point	*
 *									*
 * Output parameters:							*
 *	*px		float		X coordinate of intrsct point	*
 *	*py		float		Y coordinate of intrsct point	*
 *	*distance	float		Distance to the intrsct	point	*
 *	*iret	 	int		Return code			*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/98						*
 ***********************************************************************/
{
float	d, m, mi, b, bi;

/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( G_DIFF(x1, x2) ) {
        /*
         *  Vertical lines
         */
        *px = x1;
        *py = fy;
        *distance = G_ABS( fx - x1 );

    }
    else if ( G_DIFF(y1, y2) ) {
        /*
         *  Horizontal lines
         */
        *px = fx;
        *py = y1;
        *distance = G_ABS( fy - y1 );

    }
    else {
        /*
         *  All the rest
         */
	mi = ( y2 - y1 ) / ( x2 - x1 );
	bi = y1 - mi * x1;

	m  = - 1.0F / mi;
	b  = fy - m * fx;

	*px = ( b - bi ) / ( mi - m );
	*py = m * (*px) + b;

	d = (*px-fx)*(*px-fx) + (*py-fy)*(*py-fy);

        *distance = (float)sqrt( (double)d );

    }

}
