#include "utfcmn.h"

void uscolr ( int *icolr, int *iret )
/************************************************************************
 * uscolr								*
 * 									*
 * This subroutine sets the color on the UTF driver.			*
 * 									*
 * uscolr  ( icolr, iret )						*
 * 									*
 * Input parameters:							*
 *	*icolr		int		Color number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/97	Copied for the UTF driver		*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Save the color value.
 */
	kcolr = *icolr;

}
