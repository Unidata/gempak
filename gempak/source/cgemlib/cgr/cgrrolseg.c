#include "geminc.h"
#include "gemprm.h"

void cgr_rolseg ( float *x, float *y, float *qx, float *qy, int *rol, 
							int *iret )
/************************************************************************
 * cgr_rolseg                                                    	*
 *                                                                      *
 * This function determines whether a point (qy,qx) is to the right	*
 * of a line (or on the line) defined by the array values (y,x).	*
 *                                                                      *
 * cgr_rolseg ( x, y, qx, qy, rol, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*x	float	x-array (2 points)				*
 *	*y	float	y-array (2 points)				*
 *	*qx	float	x test point					*
 *	*qy	float	y test point					*
 *									*
 * Output parameters:                                                   *
 *	*rol	int	Flag for right of line				*
 *			    = 0 - left of line				*
 *			    = 1 - on line or right of line		*
 *	*iret	int	Return value					*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98	Create					*
 * D.W.Plummer/NCEP	 1/99	Rename from clo_rolseg.c		*
 * T. Piper/GSC		 3/99	Corrected prolog			*
 * H. Zeng/SAIC		04/05	modified pass-in parameters		*
 ***********************************************************************/
{
float	m, b, ty;
/*---------------------------------------------------------------------*/

    *iret = 0;
    *rol = 0;

    /*
     *  First check for a vertical line.
     */
    if ( G_DIFF(x[0], x[1]) )  {

	if ( y[0] < y[1] )  {
	    if ( *qx >= x[0] )  *rol = 1;
	}
	else  {
	    if ( *qx <= x[0] )  *rol = 1;
	}
    }

    /*
     *  Non-vertical lines.
     */
    else  {

	m = ( y[1] - y[0] ) / ( x[1] - x[0] );
	b = y[0] - m * x[0];

	ty = m * (*qx) + b;

	if ( m >= 0.0F )  {

	    if ( x[1] > x[0] )  {
		if ( *qy <= ty )  *rol = 1;
	    }
	    else  {
		if ( *qy >= ty )  *rol = 1;
	    }

	}
	else  {

	    if ( x[1] > x[0] )  {
		if ( *qy <= ty )  *rol = 1;
	    }
	    else  {
		if ( *qy >= ty )  *rol = 1;
	    }

	}

    }

    return;

}
