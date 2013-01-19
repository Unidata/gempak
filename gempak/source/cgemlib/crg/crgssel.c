#include "crgcmn.h"

void crg_ssel ( int elnum,  char selected, int *iret )
/************************************************************************
 * crg_ssel								*
 *                                                                      *
 * This function sets the selected flag for a specific range array    	*
 * element.           							*
 *                                                                      *
 * crg_ssel ( elnum, selected, iret )	 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number			*
 *	selected	char		selected flag			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                       -2 = Element no. out of bounds	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/98	copied from crg_get			*
 ***********************************************************************/
{
    *iret = 0;

    if ((elnum >=0 ) && ( elnum < MAX_EDITABLE_ELEMS) )
    {
	range[elnum].selected = selected;
    }
    else
    {
	*iret = -2;
    }
}
