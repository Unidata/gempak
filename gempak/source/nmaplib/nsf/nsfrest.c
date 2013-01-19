#include "nsfcmn.h"

void nsf_rest ( int *iret )
/************************************************************************
 * nsf_rest								*
 *									*
 * This routine restore the array of structure for the SFC data.	*
 *									*
 * nsf_rest ( iret )							*
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

	    sfcdt[ii] = sfcdtSave[ii];
	    ii++;
	}

}
