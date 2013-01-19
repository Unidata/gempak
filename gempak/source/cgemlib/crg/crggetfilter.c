#include "crgcmn.h"


void crg_getfilter ( int elnum, filter_t filter, int *iret )
/************************************************************************
 * crg_getfilter                                                      	*
 *                                                                      *
 * This function returns the dsiplay filter for the specified element.	*
 *                                                                      *
 * void crg_getfilter ( elnum, filter, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element index in range record	*
 *                                                                      *
 * Output parameters:                                                   *
 *      filter		filter_t	Element's display filter	*
 *      *iret           int              0 - Normal                	*
 *                                      -2 - elnum out of bounds	*
 *                                      -5 - elnum is deleted		*
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC           07/04	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
    
    *iret   = 0;
    filter[0] = '\0';

    if ( ( elnum < MAX_EDITABLE_ELEMS ) && ( elnum >= 0 ) ) {
	if ( range[elnum].ioffset >= 0 ) {
            strcpy ( filter, range[elnum].dsplyFilter );
	}
	else {
	    *iret = -5;			/* elnum is deleted */
	}
    }
    else {
	*iret = -2;			/* elnum out of bounds */
    }

}
