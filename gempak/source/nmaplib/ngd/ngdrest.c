#include "ngdcmn.h"

void ngd_rest ( int *iret )
/************************************************************************
 * ngd_rest								*
 *									*
 * This routine restore the array of structure for the GRID data.	*
 *									*
 * ngd_rest ( iret )							*
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

            grddt[ii] = grddtSave[ii];
            ii++;
        }

}
