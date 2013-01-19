#include "pscmn.h"
#include "color.h"

void pscint ( int *iret )
/************************************************************************
 * pscint								*
 *									*
 * This subroutine initalizes the graphics colors.			*
 *									*
 * pscint  ( iret )							*
 *									*
 * Output parameters:							*
 *	iret		int*		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 4/96						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Load the requested color table.
 */
	csctbl ( tblnam, iret );

}
