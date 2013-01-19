#include "nsncmn.h"

void nsn_rest ( int *iret )
/************************************************************************
 * nsn_rest								*
 *									*
 * This routine restores array of structure for the SND data.		*
 *									*
 * nsn_rest ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * M. Li/GSC		 7/00	Created					*
 ***********************************************************************/
{

	int	ii;

/*---------------------------------------------------------------------*/

	*iret = 0;
	ii = 0;

	while ( ii < MAXTMPLT ) {

	    snddt[ii] = snddtSave[ii];
	    ii++;

	}

}
