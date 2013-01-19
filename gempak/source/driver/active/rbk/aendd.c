#include "ardcmn.h"

void aendd ( int *ieop, int *iret )
/************************************************************************
 * aendd								*
 *									*
 * This subroutine closes the UTF output file.				*
 *									*
 * aendd  ( ieop, iret )						*
 *									*
 * Input parameters:							*
 *	*ieop		int	   	End plotting flag		*
 *					  0 = retain subprocess		*
 *					  1 = stop subprocess		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC 	9/98		Modified from UENDD             *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Close file at end of plot.
 */
	if  ( ( *ieop == 1 ) && opnfil )  aclosp ( iret );

}
