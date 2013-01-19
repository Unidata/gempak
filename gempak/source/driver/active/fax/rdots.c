#include "faxcmn.h"

void rdots ( int *ix, int *iy, int *ilwid, int *iret )
/************************************************************************
 * rdots								*
 *									*
 * This subroutine draws a dot to the raster image.			*
 *									*
 * rdots  ( ix, iy, ilwid, iret )					*
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
 * E.Wehner/EAI	 	 5/96	Created					*
 * E.Wehner/Eai		 3/97	Remove xsize and ysize in call to bres	*
 * S. Jacobs/NCEP	 7/97	Renamed rsopen to ropen			*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 1/98	Modified to only compute outer edge	*
 ***********************************************************************/
{

	int	jlwid, i;

/*--------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure that plot file is open.
 */
	if  ( ! opnfil ) {
	    ropen ( iret );
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
	rbrescirc ( *ix, *iy, i, iret );

}
