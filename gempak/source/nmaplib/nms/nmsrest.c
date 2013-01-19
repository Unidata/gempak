#include "nmscmn.h"

void nms_rest ( int *iret )
/************************************************************************
 * nms_rest								*
 *									*
 * This routine restores the array of structure for the MISC data.	*
 *									*
 * nms_rest ( iret )							*
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

	    mscdt[ii] = mscdtSave[ii];
	    ii++;

	}

}
