#include "faxcmn.h"

void rscolr ( int *icolr, int *iret )
/************************************************************************
 * rscolr								*
 * 									*
 * This subroutine sets the color on a raster graphics device.		*
 * 									*
 * rscolr  ( icolr, iret )						*
 * 									*
 * Input parameters:							*
 *	*icolr		int		Color number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 3/96	Fixed math for RGB calculation 		*
 * E. Wehner/EAi	 6/96	Adopted from pscolr.c for raster drv	*
 * S. Jacobs/NCEP	 7/97	Renamed rsopen to ropen			*
 * S. Jacobs/NCEP	 7/97	Changed to only set 1=on and 0=off	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Make sure plot file is open.
 */
	if  ( opnfil == 0 )  {
	    ropen ( iret );
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
