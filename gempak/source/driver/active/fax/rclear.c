#include "faxcmn.h"

void rclear ( int *iret )
/************************************************************************
 * rclear								*
 *									*
 * This subroutine clears the raster bitmap by setting all bytes	*
 * to NULL.								*
 *									*
 * rclear  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * S. Jacobs/NCEP	 7/97	Updated header; Removed check for clear	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Return if no file has been opened.
 */
	if  ( ! opnfil )  return ;

	memset ( rasimg, '\0', msize );

}
