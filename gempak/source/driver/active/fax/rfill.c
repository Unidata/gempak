#include "faxcmn.h"

void rfill ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * rfill								*
 *									*
 * This subroutine draws a filled polygon to the raster bitmap		*
 *									*
 * rfill ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points		*
 *	ix [np]		int		X coordinates			*
 *	iy [np]		int		Y coordinates			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * M. Linda/GSC		 2/97	Removed unused buffer buff[]		*
 * E. Safford/GSC	03/97	Modified to use new cgr_ routines	*
 * E. Wehner/EAi	03/97	REmoved xsize and ysize in call to scnf *
 * S. Jacobs/NCEP	 7/97	Renamed rsopen to ropen			*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 7/97	Removed color setting			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure that bitmap is open.
 */
	if  ( ! opnfil )  {
	    ropen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

	rscnfll ( np, ix, iy, iret );

}
