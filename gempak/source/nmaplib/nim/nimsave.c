#include "nimcmn.h"

void nim_save ( int *iret )
/************************************************************************
 * nim_save								*
 *									*
 * This routine saves the array of structure for the image data.	*
 *									*
 * nim_save ( iret )							*
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

	    imgSave[ii] = image[ii];
	    ii++;

	}

}
