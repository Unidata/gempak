#include "tiffcmn.h"

void tbrescirc ( int icentx, int icenty, int irad, int *iret )
/************************************************************************
 * tbrescirc								*
 *									*
 * This function draws a circle around the origin at the requested	*
 * radius.								*
 *									*
 * tbrescirc  ( icentx, icenty, irad, iret )				*
 *                                                                      *
 * Input parameters:							*
 *	icentx		int		Center of circle		*
 *	icenty		int		Center of circle		*
 *	irad		int		Radius of circle to draw	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

	int	dE, dSE, d, ix, iy, jx, jy;

/*---------------------------------------------------------------------*/

	ix = 0;
	iy = irad;
	d  = 5 - 4 * irad;

/* 
 *	Draw the circle at radius IRAD.
 */
	if  ( irad != 0 )  { 
	    jx = icentx + ix;
	    jy = icenty + iy;
	    tcircpts ( jx, jy, icentx, icenty, iret );
	    while ( iy > ix )  {
		dE  = 8 * ix + 3;
		dSE = 8 * (ix-iy) + 5;
		if  ( d <= 0 )  {
		    d += dE;
		    ix++;
		}
		else  {
		    d += dSE;
		    ix++;
		    iy--;
		}
		jx = icentx + ix;
		jy = icenty + iy;
		tcircpts ( jx, jy, icentx, icenty, iret );
	    }
	}

}
