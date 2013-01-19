#include "tiffcmn.h"

void tscolr ( int *icolr, int *iret )
/************************************************************************
 * tscolr								*
 * 									*
 * This subroutine sets the color on a raster graphics device.		*
 * 									*
 * tscolr  ( icolr, iret )						*
 * 									*
 * Input parameters:							*
 *	*icolr		int		Color number			*
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
 *	Make sure plot file is open.
 */
	if  ( opnfil == 0 )  {
	    tsopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

/*
 *	If the requested color is 101, set the data value to "off",
 *	otherwise set the value to "on".
 */
	if  ( *icolr == 101)  {

	    pixval = 0;

	}
	else  {

	    pixval = 1;

	}

}
