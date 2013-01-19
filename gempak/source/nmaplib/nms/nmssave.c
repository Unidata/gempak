#include "nmscmn.h"

void nms_save ( int *iret )
/************************************************************************
 * nms_save								*
 *									*
 * This routine saves the array of structure for the MISC data.		*
 *									*
 * nms_save ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * M. Li/GSC             7/00 	Created					*
 ***********************************************************************/
{

	int	ii;

/*---------------------------------------------------------------------*/

	*iret = 0;
	ii = 0;

	while ( ii < MAXTMPLT ) {

	    mscdtSave[ii] = mscdt[ii];
	    ii++;

	}

}
