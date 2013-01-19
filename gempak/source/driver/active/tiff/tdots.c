#include "tiffcmn.h"

void tdots ( int *ix, int *iy, int *ilwid, int *iret )
/************************************************************************
 * tdots								*
 *									*
 * This subroutine draws a dot to the raster image.			*
 *									*
 * tdots  ( ix, iy, ilwid, iret )					*
 *									*
 * Input parameters:							*
 *	*ix 		int		X coordinates			*
 *	*iy 		int		Y coordinates			*
 *	*ilwid		int		Line width			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

	int	jlwid, i;

/*--------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure that plot file is open.
 */
	if  ( ! opnfil ) {
	    tsopen ( iret );
	    if  ( *iret != G_NORMAL ) return; 
	}

/*
 *	Calculate radius of circle to draw.
 */
	jlwid = *ilwid / 2 + 1;

/*
 *	Use Bresenham's circle algorithm to compute the outer edge
 *	of the circle and fill the interior.
 */
	i = jlwid;
	tbrescirc ( *ix, *iy, i, iret );

}
