#include "faxcmn.h"

void rendd ( int *ieop, int *iret )
/************************************************************************
 * rendd								*
 *									*
 * This subroutine closes the raster image.				*
 *									*
 * rendd  ( ieop, iret )						*
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
 * E. Wehner/EAi	 4/96	Created					*
 * S. Jacobs/NCEP	 7/97	Updated header				*
 ***********************************************************************/
{

/*--------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Close file at end of plot.
 */
	if  ( *ieop == 1 )  rclosp ( iret );

}
