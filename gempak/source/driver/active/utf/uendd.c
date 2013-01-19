#include "utfcmn.h"

void uendd ( int *ieop, int *iret )
/************************************************************************
 * uendd								*
 *									*
 * This subroutine closes the UTF output file.				*
 *									*
 * uendd  ( ieop, iret )						*
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
 * E. Safford/GSC	11/96	Initial Coding				*
 * S. Jacobs/NCEP	 8/97	Removed utfplot flag			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Close file at end of plot.
 */
	if  ( ( *ieop == 1 ) && opnfil )  uclosp ( iret );

}
