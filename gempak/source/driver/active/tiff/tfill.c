#include "tiffcmn.h"

void tfill ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * tfill								*
 *									*
 * This subroutine draws a filled polygon to the raster bitmap		*
 *									*
 * tfill ( np, ix, iy, iret )						*
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
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure that bitmap is open.
 */
	if  ( ! opnfil )  {
	    tsopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

	tscnfll ( np, ix, iy, iret );

}
