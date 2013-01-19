#include "ardcmn.h"

void ascolr ( int *icolr, int *iret )
/************************************************************************
 * ascolr								*
 * 									*
 * This subroutine sets the color on the UTF driver.			*
 * 									*
 * ascolr  ( icolr, iret )						*
 * 									*
 * Input parameters:							*
 *	*icolr		int		Color number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		9/98		Modified from USCOLR            *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Save the color value.
 */
	kcolr = *icolr;

}
