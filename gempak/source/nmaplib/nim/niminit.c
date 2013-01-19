#define NIM_GLOBAL
#include "nimcmn.h"

void nim_init ( int *iret )
/************************************************************************
 * nim_init								*
 *									*
 * This routine initializes the attributes for the image data.		*
 *									*
 * nim_init ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * S. Jacobs/NCEP	 6/99	Created					*
 * S. Law/GSC		06/00	MAXIMG -> MAXTMPLT			*
 * T. Piper/SAIC	06/03	Removed nimg				*
 ***********************************************************************/
{

	int	ii;

/*---------------------------------------------------------------------*/

	*iret = 0;

	for ( ii = 0; ii < MAXTMPLT; ii++ ) {

/*
 *	    Initialize the indices.
 */
	    indimg[ii] = -1;

/*
 *	    Initialize the type, info and LUT file for each structure.
 */
	    image[ii].type[0] = CHNULL;
	    image[ii].info[0] = CHNULL;
	    image[ii].lutf[0] = CHNULL;

	}

}
