#include "faxcmn.h"

void rbrescirc ( int icentx, int icenty, int irad, int *iret )
/************************************************************************
 * rbrescirc								*
 *									*
 * This function draws a circle around the origin at the requested	*
 * radius.								*
 *									*
 * void rbrescirc  ( icentx, icenty, irad, iret )			*
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
 * E. Wehner/EAI	 6/96	Created					*
 * E. Wehner/EAi	 3/97	Check radius before drawing circle	*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 1/98	Modified to remove blanks in the circle	*
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
	    rcircpts ( jx, jy, icentx, icenty, iret );
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
		rcircpts ( jx, jy, icentx, icenty, iret );
	    }
	}

}
