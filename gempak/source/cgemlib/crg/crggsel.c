#include "crgcmn.h"

void crg_gsel ( int elnum,  char *selected, int *iret )
/************************************************************************
 * crg_gsel								*
 *                                                                      *
 * This function returns the selected flag for a particular record	*
 * in the range array.							*
 *                                                                      *
 * crg_gsel ( elnum, selected, iret)                     	 	*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *                                                                      *
 * Output parameters:                                                   *
 *	*selected	char		selected flag			*
 *      *iret           int             Return code                     *
 *                                       -2 = Element out of bounds	*
 *                                       -5 = Element is deleted 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/98	copied from crg_get			*
 * E. Safford/SAIC	04/02	add -5 error check, init *selected	*
 ***********************************************************************/
{
    *iret     = 0;
    *selected = 0;

    if ((elnum >=0 ) && ( elnum < MAX_EDITABLE_ELEMS) ) {
	if ( range[elnum].ioffset >= 0 ) {
	    *selected = range[elnum].selected;
	}
	else {
	    *iret = -5;			/* elnum is deleted */
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }

}
