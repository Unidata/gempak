#include "nsfcmn.h"

void nsf_save ( int *iret )
/************************************************************************
 * nsf_save								*
 *									*
 * This routine save the array of structure for the SFC data.		*
 *									*
 * nsf_save ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * M. Li/GSC		7/00	Created					*
 ***********************************************************************/
{

	int	ii;

/*---------------------------------------------------------------------*/

	*iret = 0;

	ii = 0;

	while (ii < MAXTMPLT ) {

	    sfcdtSave[ii] = sfcdt[ii];
	    ii++;
	}

}
