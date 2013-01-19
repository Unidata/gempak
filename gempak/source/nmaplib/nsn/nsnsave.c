#include "nsncmn.h"

void nsn_save ( int *iret )
/************************************************************************
 * nsn_save								*
 *									*
 * This routine saves array of structure for the SND data.		*
 *									*
 * nsn_save ( iret )							*
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

	    snddtSave[ii] = snddt[ii];
	    ii++;

	}

}
