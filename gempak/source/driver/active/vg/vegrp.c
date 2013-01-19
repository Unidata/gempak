#include	"vgcmn.h"

void vegrp ( int *iret )
/************************************************************************
 * vegrp								*
 *									*
 * This subroutine ends a drawing element group.			*
 *									*
 * vegrp ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/97						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Reset the group type and group number to 0.
 */
	kgtyp = 0;
	kgnum = 0;

}
