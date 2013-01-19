#include "ngdcmn.h"

void ngd_save ( int *iret )
/************************************************************************
 * ngd_save								*
 *									*
 * This routine save the array of structure for the GRID data.		*
 *									*
 * ngd_save ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * M. Li/GSC      	 7/00	Created					*
 ***********************************************************************/
{

	int	ii;

/*---------------------------------------------------------------------*/

	*iret = 0;
	ii = 0;

        while (ii < MAXTMPLT ) {

            grddtSave[ii] = grddt[ii];
            ii++;
        }

}
