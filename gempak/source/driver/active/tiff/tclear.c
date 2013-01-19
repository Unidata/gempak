#include "tiffcmn.h"

void tclear ( int *iret )
/************************************************************************
 * tclear								*
 *									*
 * This subroutine clears the raster bitmap by setting all bytes	*
 * to NULL.								*
 *									*
 * tclear  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 * S. Jacobs/NCEP	 2/99	Changed to clear image array more often	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Return if no file has been opened.
 */
	if  ( ! opnfil )  {
	    tsopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}

	memset ( rasimg, '\0', msize );

}
