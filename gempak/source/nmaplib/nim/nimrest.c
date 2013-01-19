#include "nimcmn.h"

void nim_rest ( int *iret )
/************************************************************************
 * nim_rest								*
 *									*
 * This routine restores the array of structure for the image data.	*
 *									*
 * nim_rest ( iret )							*
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

	    image[ii] = imgSave[ii];
	    ii++;

	}

}
