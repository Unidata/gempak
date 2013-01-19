#include "tiffcmn.h"

void tline ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * tline								*
 *									*
 * This subroutine draws lines to the raster image.			*
 *									*
 * tline  ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	ix[np]		int		X coordinates			*
 *	iy[np]		int		Y coordinates			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

	int	i, j, ie, idiff, idelx, idely,
		incx, incy, jx, jy, ier;

/*---------------------------------------------------------------------*/
    *iret = G_NORMAL;

/*
 *	Make sure the raster image is open.
 */
	if  ( ! opnfil )  {
	    tsopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*
 *	Rasterize the line.
 *
 *	This algorithm is due to Bresenham as given in "Principles of
 *	Interactive Computer Graphics" by Newman & Sproull.
 */
	for ( i = 1; i < *np; i++ )  {

/*
 *	    Compute the delta and direction for the X coord.
 */
	    idelx = ix[i] - ix[i-1];
	    if  ( idelx < 0 )  {
		incx  = -1;
		idelx = -idelx;
	    }
	    else {
		incx  = 1;
	    }

/*
 *	    Compute the delta and direction for the Y coord.
 */
	    idely = iy[i] - iy[i-1];
	    if  ( idely < 0 )  {
		incy  = -1;
		idely = -idely;
	    }
	    else {
		incy  = 1;
	    }

/*
 *	    Plot lines with slope between 0 and 1, inclusive. This
 *	    handles horizontal and diagonal lines.
 */
	    if  ( idelx >= idely )  {
		ie    = 2 * idely - idelx;
		jy    = iy[i-1];
		idiff = idely - idelx;

/*
 *		This loop counts forward or backward depending on the
 *		the sign of INCX.
 */
		for ( j = ix[i-1];
		      ( incx > 0 ? j <= ix[i] : j >= ix[i] );
		      j += incx )  {

/*
 *		    Plot the point to the raster image.
 */
		    twrpxl ( j, jy, &ier );

/*
 *		    Compute the offset for the next point.
 */
		    if  ( ie > 0 )  {
			jy = jy + incy;
			ie = ie + 2 * idiff;
		    }
		    else  {
			ie = ie + 2 * idely;
		    }
		}
	    }
/*
 *	    Plot lines with slope greater than 1. This handles
 *	    vertical lines.
 */
	    else {
		ie    = 2 * idelx - idely;
		jx    = ix[i-1];
		idiff = idelx - idely;

/*
 *		This loop counts forward or backward depending on the
 *		the sign of INCY.
 */
		for ( j = iy[i-1];
		      ( incy > 0 ? j <= iy[i] : j >= iy[i] );
		      j += incy )  {

/*
 *		    Plot the point to the raster image.
 */
		    twrpxl ( jx, j, &ier );

/*
 *		    Compute the offset for the next point.
 */
		    if  ( ie > 0 )  {
			jx = jx + incx;
			ie = ie + 2 * idiff;
		    }
		    else  {
			ie = ie + 2 * idelx;
		    }
		}
	    }
	}

}
