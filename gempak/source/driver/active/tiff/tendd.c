#include "tiffcmn.h"

void tendd ( int *ieop, int *iret )
/************************************************************************
 * tendd								*
 *									*
 * This subroutine closes the raster image.				*
 *									*
 * tendd  ( ieop, iret )						*
 *									*
 * Input parameters:							*
 *	*ieop		int		End plotting flag		*
 *					  0 = retain subprocess		*
 *					  1 = stop subprocess		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

/*--------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Close file at end of plot.
 */
	if  ( *ieop == 1 )  tclosp ( iret );

}
